// embed.cc -- Go frontend go:embed handling.

// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "operator.h"
#include "go-diagnostics.h"
#include "lex.h"
#include "types.h"
#include "expressions.h"
#include "gogo.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

// Read a file into *DATA.  Returns false on error.

bool
Gogo::read_file(const char* filename, Location loc, std::string* data)
{
  int fd = open(filename, O_RDONLY | O_BINARY);
  if (fd < 0)
    {
      go_error_at(loc, "%s: %m", filename);
      return false;
    }

  struct stat st;
  if (fstat(fd, &st) < 0)
    {
      go_error_at(loc, "%s: %m", filename);
      return false;
    }
  off_t want = st.st_size;

  // Most files read here are going to be incorporated into the object file
  // and then the executable.  Set a limit on the size we will accept.
  if (want > 2000000000)
    {
      go_error_at(loc, "%s: file too large", filename);
      return false;
    }

  data->resize(want);
  off_t got = 0;
  while (want > 0)
    {
      // C++11 requires that std::string use contiguous bytes, so this
      // is safe.
      ssize_t n = read(fd, &(*data)[got], want);
      if (n < 0)
	{
	  close(fd);
	  go_error_at(loc, "%s: %m", filename);
	  return false;
	}
      if (n == 0)
	{
	  data->resize(got);
	  break;
	}
      got += n;
      want -= n;
    }

  close(fd);
  return true;
}

// A JSON value as read from an embedcfg file.  For our purposes a
// JSON value is a string, or a list of strings, or a mapping from
// strings to values.  We don't expect any numbers.  We also don't
// expect an array of anything other than strings; that is, we don't
// accept an array of general JSON values.

class Json_value
{
 public:
  // The types of values.
  enum Json_value_classification
    {
      JSON_VALUE_UNKNOWN,
      JSON_VALUE_STRING,
      JSON_VALUE_ARRAY,
      JSON_VALUE_MAP
    };

  Json_value()
    : classification_(JSON_VALUE_UNKNOWN), string_(), array_(), map_()
  { }

  ~Json_value();

  Json_value_classification
  classification() const
  { return this->classification_; }

  // Set to a string value.
  void
  set_string(const std::string& str)
  {
    go_assert(this->classification_ == JSON_VALUE_UNKNOWN);
    this->classification_ = JSON_VALUE_STRING;
    this->string_ = str;
  }

  // Start an array value.
  void
  start_array()
  {
    go_assert(this->classification_ == JSON_VALUE_UNKNOWN);
    this->classification_ = JSON_VALUE_ARRAY;
  }

  // Add an array entry.
  void
  add_array_entry(const std::string& s)
  {
    go_assert(this->classification_ == JSON_VALUE_ARRAY);
    this->array_.push_back(s);
  }

  // Start a map value.
  void
  start_map()
  {
    go_assert(this->classification_ == JSON_VALUE_UNKNOWN);
    this->classification_ = JSON_VALUE_MAP;
  }

  // Add a map entry.
  void
  add_map_entry(const std::string& key, Json_value* val)
  {
    go_assert(this->classification_ == JSON_VALUE_MAP);
    this->map_[key] = val;
  }

  // Return the strings from a string value.
  const std::string&
  to_string() const
  {
    go_assert(this->classification_ == JSON_VALUE_STRING);
    return this->string_;
  }

  // Fetch a vector of strings, and drop them from the JSON value.
  void
  get_and_clear_array(std::vector<std::string>* v)
  {
    go_assert(this->classification_ == JSON_VALUE_ARRAY);
    std::swap(*v, this->array_);
  }

  // Look up a map entry.  Returns NULL if not found.
  Json_value*
  lookup_map_entry(const std::string& key);

  // Iterate over a map.
  typedef Unordered_map(std::string, Json_value*)::iterator map_iterator;

  map_iterator
  map_begin()
  {
    go_assert(this->classification_ == JSON_VALUE_MAP);
    return this->map_.begin();
  }

  map_iterator
  map_end()
  { return this->map_.end(); }

 private:
  // Classification.
  Json_value_classification classification_;
  // A string, for JSON_VALUE_STRING.
  std::string string_;
  // Array, for JSON_VALUE_ARRAY.
  std::vector<std::string> array_;
  // Mapping, for JSON_VALUE_MAP.
  Unordered_map(std::string, Json_value*) map_;
};

// Delete a JSON value.

Json_value::~Json_value()
{
  if (this->classification_ == JSON_VALUE_MAP)
    {
      for (map_iterator p = this->map_begin();
	   p != this->map_end();
	   ++p)
	delete p->second;
    }
}

// Look up a map entry in a JSON value.

Json_value*
Json_value::lookup_map_entry(const std::string& key)
{
  go_assert(this->classification_ == JSON_VALUE_MAP);
  Unordered_map(std::string, Json_value*)::iterator p = this->map_.find(key);
  if (p == this->map_.end())
    return NULL;
  return p->second;
}

// Manage reading the embedcfg file.

class Embedcfg_reader
{
 public:
  Embedcfg_reader(const char* filename)
    : filename_(filename), data_(), p_(NULL), pend_(NULL)
  {}

  // Read the contents of FILENAME.  Return whether it succeeded.
  bool
  initialize_from_file();

  // Read a JSON object.
  bool
  read_object(Json_value*);

  // Report an error if not at EOF.
  void
  check_eof();

  // Report an error for the embedcfg file.
  void
  error(const char* msg);

 private:
  bool
  read_value(Json_value*);

  bool
  read_array(Json_value*);

  bool
  read_string(std::string*);

  bool
  skip_whitespace(bool eof_ok);

  // File name.
  const char* filename_;
  // File contents.
  std::string data_;
  // Next character to process.
  const char *p_;
  // End of data.
  const char *pend_;
};

// Read the embedcfg file.

void
Gogo::read_embedcfg(const char *filename)
{
  class Embedcfg_reader r(filename);
  if (!r.initialize_from_file())
    return;

  Json_value val;
  if (!r.read_object(&val))
    return;

  r.check_eof();

  if (val.classification() != Json_value::JSON_VALUE_MAP)
    {
      r.error("invalid embedcfg: not a JSON object");
      return;
    }

  Json_value* patterns = val.lookup_map_entry("Patterns");
  if (patterns == NULL)
    {
      r.error("invalid embedcfg: missing Patterns");
      return;
    }
  if (patterns->classification() != Json_value::JSON_VALUE_MAP)
    {
      r.error("invalid embedcfg: Patterns is not a JSON object");
      return;
    }

  Json_value* files = val.lookup_map_entry("Files");
  if (files == NULL)
    {
      r.error("invalid embedcfg: missing Files");
      return;
    }
  if (files->classification() != Json_value::JSON_VALUE_MAP)
    {
      r.error("invalid embedcfg: Files is not a JSON object");
      return;
    }

  for (Json_value::map_iterator p = patterns->map_begin();
       p != patterns->map_end();
       ++p)
    {
      if (p->second->classification() != Json_value::JSON_VALUE_ARRAY)
	{
	  r.error("invalid embedcfg: Patterns entry is not an array");
	  return;
	}
      std::vector<std::string> files;
      p->second->get_and_clear_array(&files);

      std::pair<std::string, std::vector<std::string> > val;
      val.first = p->first;
      std::pair<Embed_patterns::iterator, bool> ins =
	this->embed_patterns_.insert(val);
      if (!ins.second)
	{
	  r.error("invalid embedcfg: duplicate Patterns entry");
	  return;
	}
      std::swap(ins.first->second, files);
    }

  for (Json_value::map_iterator p = files->map_begin();
       p != files->map_end();
       ++p)
    {
      if (p->second->classification() != Json_value::JSON_VALUE_STRING)
	{
	  r.error("invalid embedcfg: Files entry is not a string");
	  return;
	}
      this->embed_files_[p->first] = p->second->to_string();
    }
}

// Read the contents of FILENAME into this->data_.  Returns whether it
// succeeded.

bool
Embedcfg_reader::initialize_from_file()
{
  if (!Gogo::read_file(this->filename_, Linemap::unknown_location(),
		       &this->data_))
    return false;
  if (this->data_.empty())
    {
      this->error("empty file");
      return false;
    }
  this->p_ = this->data_.data();
  this->pend_ = this->p_ + this->data_.size();
  return true;
}

// Read a JSON object into VAL.  Return whether it succeeded.

bool
Embedcfg_reader::read_object(Json_value* val)
{
  if (!this->skip_whitespace(false))
    return false;
  if (*this->p_ != '{')
    {
      this->error("expected %<{%>");
      return false;
    }
  ++this->p_;

  val->start_map();

  if (!this->skip_whitespace(false))
    return false;
  if (*this->p_ == '}')
    {
      ++this->p_;
      return true;
    }

  while (true)
    {
      if (!this->skip_whitespace(false))
	return false;
      if (*this->p_ != '"')
	{
	  this->error("expected %<\"%>");
	  return false;
	}

      std::string key;
      if (!this->read_string(&key))
	return false;

      if (!this->skip_whitespace(false))
	return false;
      if (*this->p_ != ':')
	{
	  this->error("expected %<:%>");
	  return false;
	}
      ++this->p_;

      Json_value* subval = new Json_value();
      if (!this->read_value(subval))
	return false;

      val->add_map_entry(key, subval);

      if (!this->skip_whitespace(false))
	return false;
      if (*this->p_ == '}')
	{
	  ++this->p_;
	  return true;
	}
      if (*this->p_ != ',')
	{
	  this->error("expected %<,%> or %<}%>");
	  return false;
	}
      ++this->p_;
    }
}

// Read a JSON array into VAL.  Return whether it succeeded.

bool
Embedcfg_reader::read_array(Json_value* val)
{
  if (!this->skip_whitespace(false))
    return false;
  if (*this->p_ != '[')
    {
      this->error("expected %<[%>");
      return false;
    }
  ++this->p_;

  val->start_array();

  if (!this->skip_whitespace(false))
    return false;
  if (*this->p_ == ']')
    {
      ++this->p_;
      return true;
    }

  while (true)
    {
      // If we were parsing full JSON we would call read_value here,
      // not read_string.

      std::string s;
      if (!this->read_string(&s))
	return false;

      val->add_array_entry(s);

      if (!this->skip_whitespace(false))
	return false;
      if (*this->p_ == ']')
	{
	  ++this->p_;
	  return true;
	}
      if (*this->p_ != ',')
	{
	  this->error("expected %<,%> or %<]%>");
	  return false;
	}
      ++this->p_;
    }
}

// Read a JSON value into VAL.  Return whether it succeeded.

bool
Embedcfg_reader::read_value(Json_value* val)
{
  if (!this->skip_whitespace(false))
    return false;
  switch (*this->p_)
    {
    case '"':
      {
	std::string s;
	if (!this->read_string(&s))
	  return false;
	val->set_string(s);
	return true;
      }

    case '{':
      return this->read_object(val);

    case '[':
      return this->read_array(val);

    default:
      this->error("invalid JSON syntax");
      return false;
    }
}

// Read a JSON string.  Return whether it succeeded.

bool
Embedcfg_reader::read_string(std::string* str)
{
  if (!this->skip_whitespace(false))
    return false;
  if (*this->p_ != '"')
    {
      this->error("expected %<\"%>");
      return false;
    }
  ++this->p_;

  str->clear();
  while (this->p_ < this->pend_ && *this->p_ != '"')
    {
      if (*this->p_ != '\\')
	{
	  str->push_back(*this->p_);
	  ++this->p_;
	  continue;
	}

      ++this->p_;
      if (this->p_ >= this->pend_)
	{
	  this->error("unterminated string");
	  return false;
	}
      switch (*this->p_)
	{
	case '"': case '\\': case '/':
	  str->push_back(*this->p_);
	  ++this->p_;
	  break;

	case 'b':
	  str->push_back('\b');
	  ++this->p_;
	  break;

	case 'f':
	  str->push_back('\f');
	  ++this->p_;
	  break;

	case 'n':
	  str->push_back('\n');
	  ++this->p_;
	  break;

	case 'r':
	  str->push_back('\r');
	  ++this->p_;
	  break;

	case 't':
	  str->push_back('\t');
	  ++this->p_;
	  break;

	case 'u':
	  {
	    ++this->p_;
	    unsigned int rune = 0;
	    for (int i = 0; i < 4; i++)
	      {
		if (this->p_ >= this->pend_)
		  {
		    this->error("unterminated string");
		    return false;
		  }
		unsigned char c = *this->p_;
		++this->p_;
		rune <<= 4;
		if (c >= '0' && c <= '9')
		  rune += c - '0';
		else if (c >= 'A' && c <= 'F')
		  rune += c - 'A' + 10;
		else if (c >= 'a' && c <= 'f')
		  rune += c - 'a' + 10;
		else
		  {
		    this->error("invalid hex digit");
		    return false;
		  }
	      }
	    Lex::append_char(rune, false, str, Linemap::unknown_location());
	  }
	  break;

	default:
	  this->error("unrecognized string escape");
	  return false;
	}
    }

  if (*this->p_ == '"')
    {
      ++this->p_;
      return true;
    }

  this->error("unterminated string");
  return false;
}

// Report an error if not at EOF.

void
Embedcfg_reader::check_eof()
{
  if (this->skip_whitespace(true))
    this->error("extraneous data at end of file");
}

// Skip whitespace.  Return whether there is more to read.

bool
Embedcfg_reader::skip_whitespace(bool eof_ok)
{
  while (this->p_ < this->pend_)
    {
      switch (*this->p_)
	{
	case ' ': case '\t': case '\n': case '\r':
	  ++this->p_;
	  break;
	default:
	  return true;
	}
    }
  if (!eof_ok)
    this->error("unexpected EOF");
  return false;
}

// Report an error.

void
Embedcfg_reader::error(const char* msg)
{
  if (!this->data_.empty() && this->p_ != NULL)
    go_error_at(Linemap::unknown_location(),
		"%<-fgo-embedcfg%>: %s: %lu: %s",
		this->filename_,
		static_cast<unsigned long>(this->p_ - this->data_.data()),
		msg);
  else
    go_error_at(Linemap::unknown_location(),
		"%<-fgo-embedcfg%>: %s: %s",
		this->filename_, msg);
}

// Implement the sort order for a list of embedded files, as discussed
// at the docs for embed.FS.

class Embedfs_sort
{
 public:
  bool
  operator()(const std::string& p1, const std::string& p2) const;

 private:
  void
  split(const std::string&, size_t*, size_t*, size_t*) const;
};

bool
Embedfs_sort::operator()(const std::string& p1, const std::string& p2) const
{
  size_t dirlen1, elem1, elemlen1;
  this->split(p1, &dirlen1, &elem1, &elemlen1);
  size_t dirlen2, elem2, elemlen2;
  this->split(p2, &dirlen2, &elem2, &elemlen2);

  if (dirlen1 == 0)
    {
      if (dirlen2 > 0)
	{
	  int i = p2.compare(0, dirlen2, ".");
	  if (i != 0)
	    return i > 0;
	}
    }
  else if (dirlen2 == 0)
    {
      int i = p1.compare(0, dirlen1, ".");
      if (i != 0)
	return i < 0;
    }
  else
    {
      int i = p1.compare(0, dirlen1, p2, 0, dirlen2);
      if (i != 0)
	return i < 0;
    }

  int i = p1.compare(elem1, elemlen1, p2, elem2, elemlen2);
  return i < 0;
}

// Pick out the directory and file name components for comparison.

void
Embedfs_sort::split(const std::string& s, size_t* dirlen, size_t* elem,
		    size_t* elemlen) const
{
  size_t len = s.size();
  if (len > 0 && s[len - 1] == '/')
    --len;
  size_t slash = s.rfind('/', len - 1);
  if (slash == std::string::npos)
    {
      *dirlen = 0;
      *elem = 0;
      *elemlen = len;
    }
  else
    {
      *dirlen = slash;
      *elem = slash + 1;
      *elemlen = len - (slash + 1);
    }
}

// Convert the go:embed directives for a variable into an initializer
// for that variable.

Expression*
Gogo::initializer_for_embeds(Type* type,
			     const std::vector<std::string>* embeds,
			     Location loc)
{
  if (this->embed_patterns_.empty())
    {
      go_error_at(loc,
		  ("invalid go:embed: build system did not "
		   "supply embed configuration"));
      return Expression::make_error(loc);
    }

  type = type->unalias();

  enum {
    EMBED_STRING = 0,
    EMBED_BYTES = 1,
    EMBED_FS = 2
  } embed_kind;

  const Named_type* nt = type->named_type();
  if (nt != NULL
      && nt->named_object()->package() != NULL
      && nt->named_object()->package()->pkgpath() == "embed"
      && nt->name() == "FS")
    embed_kind = EMBED_FS;
  else if (type->is_string_type())
    embed_kind = EMBED_STRING;
  else if (type->is_slice_type()
	   && type->array_type()->element_type()->integer_type() != NULL
	   && type->array_type()->element_type()->integer_type()->is_byte())
    embed_kind = EMBED_BYTES;
  else
    {
      go_error_at(loc, "invalid type for go:embed");
      return Expression::make_error(loc);
    }

  // The patterns in the go:embed directive(s) are in EMBEDS.  Find
  // them in the patterns in the embedcfg file.

  Unordered_set(std::string) have;
  std::vector<std::string> paths;
  for (std::vector<std::string>::const_iterator pe = embeds->begin();
       pe != embeds->end();
       pe++)
    {
      Embed_patterns::const_iterator pp = this->embed_patterns_.find(*pe);
      if (pp == this->embed_patterns_.end())
	{
	  go_error_at(loc,
		      ("invalid go:embed: build system did not "
		       "map pattern %qs"),
		      pe->c_str());
	  continue;
	}

      // Each pattern in the embedcfg file maps to a list of file
      // names.  Add those file names to PATHS.
      for (std::vector<std::string>::const_iterator pf = pp->second.begin();
	   pf != pp->second.end();
	   pf++)
	{
	  if (this->embed_files_.find(*pf) == this->embed_files_.end())
	    {
	      go_error_at(loc,
			  ("invalid go:embed: build system did not "
			   "map file %qs"),
			  pf->c_str());
	      continue;
	    }

	  std::pair<Unordered_set(std::string)::iterator, bool> ins
	    = have.insert(*pf);
	  if (ins.second)
	    {
	      const std::string& path(*pf);
	      paths.push_back(path);

	      if (embed_kind == EMBED_FS)
		{
		  // Add each required directory, with a trailing slash.
		  size_t i = std::string::npos;
		  while (i > 0)
		    {
		      i = path.rfind('/', i);
		      if (i == std::string::npos)
			break;
		      std::string dir = path.substr(0, i + 1);
		      ins = have.insert(dir);
		      if (ins.second)
			paths.push_back(dir);
		      --i;
		    }
		}
	    }
	}
    }

  if (embed_kind == EMBED_STRING || embed_kind == EMBED_BYTES)
    {
      if (paths.size() > 1)
	{
	  go_error_at(loc,
		      ("invalid go:embed: multiple files for "
		       "string or byte slice"));;
	  return Expression::make_error(loc);
	}

      std::string data;
      if (!Gogo::read_file(this->embed_files_[paths[0]].c_str(), loc, &data))
	return Expression::make_error(loc);

      Expression* e = Expression::make_string(data, loc);
      if (embed_kind == EMBED_BYTES)
	e = Expression::make_cast(type, e, loc);
      return e;
    }

  std::sort(paths.begin(), paths.end(), Embedfs_sort());

  if (type->struct_type() == NULL
      || type->struct_type()->field_count() != 1)
    {
      go_error_at(loc,
		  ("internal error: embed.FS should be struct type "
		   "with one field"));
      return Expression::make_error(loc);
    }

  Type* ptr_type = type->struct_type()->field(0)->type();
  if (ptr_type->points_to() == NULL)
    {
      go_error_at(loc,
		  "internal error: embed.FS struct field should be pointer");
      return Expression::make_error(loc);
    }

  Type* slice_type = ptr_type->points_to();
  if (!slice_type->is_slice_type())
    {
      go_error_at(loc,
		  ("internal error: embed.FS struct field should be "
		   "pointer to slice"));
      return Expression::make_error(loc);
    }

  Type* file_type = slice_type->array_type()->element_type();
  if (file_type->struct_type() == NULL
      || (file_type->struct_type()->find_local_field(".embed.name", NULL)
	  == NULL)
      || (file_type->struct_type()->find_local_field(".embed.data", NULL)
	  == NULL))
    {
      go_error_at(loc,
		  ("internal error: embed.FS slice element should be struct "
		   "with name and data fields"));
      return Expression::make_error(loc);
    }

  const Struct_field_list* file_fields = file_type->struct_type()->fields();
  Expression_list* file_vals = new(Expression_list);
  file_vals->reserve(paths.size());
  for (std::vector<std::string>::const_iterator pp = paths.begin();
       pp != paths.end();
       ++pp)
    {
      std::string data;
      if ((*pp)[pp->size() - 1] != '/')
	{
	  if (!Gogo::read_file(this->embed_files_[*pp].c_str(), loc, &data))
	    return Expression::make_error(loc);
	}

      Expression_list* field_vals = new(Expression_list);
      for (Struct_field_list::const_iterator pf = file_fields->begin();
	   pf != file_fields->end();
	   ++pf)
	{
	  if (pf->is_field_name(".embed.name"))
	    field_vals->push_back(Expression::make_string(*pp, loc));
	  else if (pf->is_field_name(".embed.data"))
	    field_vals->push_back(Expression::make_string(data, loc));
	  else
	    {
	      // FIXME: The embed.file type has a hash field, which is
	      // currently unused.  We should fill it in, but don't.
	      // The hash is a SHA256, and we don't have convenient
	      // SHA256 code.  Do this later when the field is
	      // actually used.
	      field_vals->push_back(NULL);
	    }
	}

      Expression* file_val =
	Expression::make_struct_composite_literal(file_type, field_vals, loc);
      file_vals->push_back(file_val);
    }

  Expression* slice_init =
    Expression::make_slice_composite_literal(slice_type, file_vals, loc);
  Expression* fs_init = Expression::make_heap_expression(slice_init, loc);
  Expression_list* fs_vals = new Expression_list();
  fs_vals->push_back(fs_init);
  return Expression::make_struct_composite_literal(type, fs_vals, loc);
}
