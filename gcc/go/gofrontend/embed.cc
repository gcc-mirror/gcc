// embed.cc -- Go frontend go:embed handling.

// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "operator.h"
#include "go-diagnostics.h"
#include "lex.h"
#include "gogo.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

// Read a file into *DATA.  Returns false on error.

static bool
read_file(const char* filename, Location loc, std::string* data)
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

  // TODO: Actually do something with patterns and files.
}

// Read the contents of FILENAME into this->data_.  Returns whether it
// succeeded.

bool
Embedcfg_reader::initialize_from_file()
{
  if (!read_file(this->filename_, Linemap::unknown_location(), &this->data_))
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

// Return whether the current file imports "embed".

bool
Gogo::is_embed_imported() const
{
  Packages::const_iterator p = this->packages_.find("embed");
  if (p == this->packages_.end())
    return false;

  // We track current file imports in the package aliases, where a
  // typical import will just list the package name in aliases.  So
  // the package has been imported if there is at least one alias.
  return !p->second->aliases().empty();
}
