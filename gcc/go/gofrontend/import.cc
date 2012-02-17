// import.cc -- Go frontend import declarations.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "filenames.h"
#include "simple-object.h"

#include "go-c.h"
#include "gogo.h"
#include "lex.h"
#include "types.h"
#include "export.h"
#include "import.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif

// The list of paths we search for import files.

static std::vector<std::string> search_path;

// Add a directory to the search path.  This is called from the option
// handling language hook.

GO_EXTERN_C
void
go_add_search_path(const char* path)
{
  search_path.push_back(std::string(path));
}

// Find import data.  This searches the file system for FILENAME and
// returns a pointer to a Stream object to read the data that it
// exports.  If the file is not found, it returns NULL.

// When FILENAME is not an absolute path and does not start with ./ or
// ../, we use the search path provided by -I and -L options.

// When FILENAME does not exist, we try modifying FILENAME to find the
// file.  We use the first of these which exists:
//   * We append ".gox".
//   * We turn the base of FILENAME into libFILENAME.so.
//   * We turn the base of FILENAME into libFILENAME.a.
//   * We append ".o".

// When using a search path, we apply each of these transformations at
// each entry on the search path before moving on to the next entry.
// If the file exists, but does not contain any Go export data, we
// stop; we do not keep looking for another file with the same name
// later in the search path.

Import::Stream*
Import::open_package(const std::string& filename, Location location)
{
  bool is_local;
  if (IS_ABSOLUTE_PATH(filename))
    is_local = true;
  else if (filename[0] == '.' && IS_DIR_SEPARATOR(filename[1]))
    is_local = true;
  else if (filename[0] == '.'
	   && filename[1] == '.'
	   && IS_DIR_SEPARATOR(filename[2]))
    is_local = true;
  else
    is_local = false;
  if (!is_local)
    {
      for (std::vector<std::string>::const_iterator p = search_path.begin();
	   p != search_path.end();
	   ++p)
	{
	  std::string indir = *p;
	  if (!indir.empty() && indir[indir.size() - 1] != '/')
	    indir += '/';
	  indir += filename;
	  Stream* s = Import::try_package_in_directory(indir, location);
	  if (s != NULL)
	    return s;
	}
    }

  Stream* s = Import::try_package_in_directory(filename, location);
  if (s != NULL)
    return s;

  return NULL;
}

// Try to find the export data for FILENAME.

Import::Stream*
Import::try_package_in_directory(const std::string& filename,
				 Location location)
{
  std::string found_filename = filename;
  int fd = open(found_filename.c_str(), O_RDONLY | O_BINARY);

  if (fd >= 0)
    {
      struct stat s;
      if (fstat(fd, &s) >= 0 && S_ISDIR(s.st_mode))
	{
	  close(fd);
	  fd = -1;
	  errno = EISDIR;
	}
    }

  if (fd < 0)
    {
      if (errno != ENOENT && errno != EISDIR)
	warning_at(location, 0, "%s: %m", filename.c_str());

      fd = Import::try_suffixes(&found_filename);
      if (fd < 0)
	return NULL;
    }

  // The export data may not be in this file.
  Stream* s = Import::find_export_data(found_filename, fd, location);
  if (s != NULL)
    return s;

  close(fd);

  error_at(location, "%s exists but does not contain any Go export data",
	   found_filename.c_str());

  return NULL;
}

// Given import "*PFILENAME", where *PFILENAME does not exist, try
// various suffixes.  If we find one, set *PFILENAME to the one we
// found.  Return the open file descriptor.

int
Import::try_suffixes(std::string* pfilename)
{
  std::string filename = *pfilename + ".gox";
  int fd = open(filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  const char* basename = lbasename(pfilename->c_str());
  size_t basename_pos = basename - pfilename->c_str();
  filename = pfilename->substr(0, basename_pos) + "lib" + basename + ".so";
  fd = open(filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = pfilename->substr(0, basename_pos) + "lib" + basename + ".a";
  fd = open(filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  filename = *pfilename + ".o";
  fd = open(filename.c_str(), O_RDONLY | O_BINARY);
  if (fd >= 0)
    {
      *pfilename = filename;
      return fd;
    }

  return -1;
}

// Look for export data in the file descriptor FD.

Import::Stream*
Import::find_export_data(const std::string& filename, int fd,
			 Location location)
{
  // See if we can read this as an object file.
  Import::Stream* stream = Import::find_object_export_data(filename, fd, 0,
							   location);
  if (stream != NULL)
    return stream;

  const int len = MAX(Export::v1_magic_len, Import::archive_magic_len);

  if (lseek(fd, 0, SEEK_SET) < 0)
    {
      error_at(location, "lseek %s failed: %m", filename.c_str());
      return NULL;
    }

  char buf[len];
  ssize_t c = read(fd, buf, len);
  if (c < len)
    return NULL;

  // Check for a file containing nothing but Go export data.
  if (memcmp(buf, Export::v1_magic, Export::v1_magic_len) == 0)
    return new Stream_from_file(fd);

  // See if we can read this as an archive.
  if (Import::is_archive_magic(buf))
    return Import::find_archive_export_data(filename, fd, location);

  return NULL;
}

// Look for export data in a simple_object.

Import::Stream*
Import::find_object_export_data(const std::string& filename,
				int fd,
				off_t offset,
				Location location)
{
  char *buf;
  size_t len;
  int err;
  const char *errmsg = go_read_export_data(fd, offset, &buf, &len, &err);
  if (errmsg != NULL)
    {
      if (err == 0)
	error_at(location, "%s: %s", filename.c_str(), errmsg);
      else
	error_at(location, "%s: %s: %s", filename.c_str(), errmsg,
		 xstrerror(err));
      return NULL;
    }

  if (buf == NULL)
    return NULL;

  return new Stream_from_buffer(buf, len);
}

// Class Import.

// Construct an Import object.  We make the builtin_types_ vector
// large enough to hold all the builtin types.

Import::Import(Stream* stream, Location location)
  : gogo_(NULL), stream_(stream), location_(location), package_(NULL),
    add_to_globals_(false),
    builtin_types_((- SMALLEST_BUILTIN_CODE) + 1),
    types_()
{
}

// Import the data in the associated stream.

Package*
Import::import(Gogo* gogo, const std::string& local_name,
	       bool is_local_name_exported)
{
  // Hold on to the Gogo structure.  Otherwise we need to pass it
  // through all the import functions, because we need it when reading
  // a type.
  this->gogo_ = gogo;

  // A stream of export data can include data from more than one input
  // file.  Here we loop over each input file.
  Stream* stream = this->stream_;
  while (!stream->at_eof() && !stream->saw_error())
    {
      // The vector of types is package specific.
      this->types_.clear();

      stream->require_bytes(this->location_, Export::v1_magic,
			    Export::v1_magic_len);

      this->require_c_string("package ");
      std::string package_name = this->read_identifier();
      this->require_c_string(";\n");

      this->require_c_string("prefix ");
      std::string unique_prefix = this->read_identifier();
      this->require_c_string(";\n");

      this->package_ = gogo->add_imported_package(package_name, local_name,
						  is_local_name_exported,
						  unique_prefix,
						  this->location_,
						  &this->add_to_globals_);
      if (this->package_ == NULL)
	{
	  stream->set_saw_error();
	  return NULL;
	}

      this->require_c_string("priority ");
      std::string priority_string = this->read_identifier();
      int prio;
      if (!this->string_to_int(priority_string, false, &prio))
	return NULL;
      this->package_->set_priority(prio);
      this->require_c_string(";\n");

      if (stream->match_c_string("import "))
	this->read_import_init_fns(gogo);

      // Loop over all the input data for this package.
      while (!stream->saw_error())
	{
	  if (stream->match_c_string("const "))
	    this->import_const();
	  else if (stream->match_c_string("type "))
	    this->import_type();
	  else if (stream->match_c_string("var "))
	    this->import_var();
	  else if (stream->match_c_string("func "))
	    this->import_func(this->package_);
	  else if (stream->match_c_string("checksum "))
	    break;
	  else
	    {
	      error_at(this->location_,
		       ("error in import data at %d: "
			"expected %<const%>, %<type%>, %<var%>, "
			"%<func%>, or %<checksum%>"),
		       stream->pos());
	      stream->set_saw_error();
	      return NULL;
	    }
	}

      // We currently ignore the checksum.  In the future we could
      // store the checksum somewhere in the generated object and then
      // verify that the checksum matches at link time or at dynamic
      // load time.
      this->require_c_string("checksum ");
      stream->advance(Export::v1_checksum_len * 2);
      this->require_c_string(";\n");
    }

  return this->package_;
}

// Read the list of import control functions.

void
Import::read_import_init_fns(Gogo* gogo)
{
  this->require_c_string("import");
  while (!this->match_c_string(";"))
    {
      this->require_c_string(" ");
      std::string package_name = this->read_identifier();
      this->require_c_string(" ");
      std::string init_name = this->read_identifier();
      this->require_c_string(" ");
      std::string prio_string = this->read_identifier();
      int prio;
      if (!this->string_to_int(prio_string, false, &prio))
	return;
      gogo->add_import_init_fn(package_name, init_name, prio);
    }
  this->require_c_string(";\n");
}

// Import a constant.

void
Import::import_const()
{
  std::string name;
  Type* type;
  Expression* expr;
  Named_constant::import_const(this, &name, &type, &expr);
  Typed_identifier tid(name, type, this->location_);
  Named_object* no = this->package_->add_constant(tid, expr);
  if (this->add_to_globals_)
    this->gogo_->add_named_object(no);
}

// Import a type.

void
Import::import_type()
{
  Named_type* type;
  Named_type::import_named_type(this, &type);

  // The named type has been added to the package by the type import
  // process.  Here we need to make it visible to the parser, and it
  // to the global bindings if necessary.
  type->set_is_visible();

  if (this->add_to_globals_)
    this->gogo_->add_named_type(type);
}

// Import a variable.

void
Import::import_var()
{
  std::string name;
  Type* type;
  Variable::import_var(this, &name, &type);
  Variable* var = new Variable(type, NULL, true, false, false,
			       this->location_);
  Named_object* no;
  no = this->package_->add_variable(name, var);
  if (this->add_to_globals_)
    this->gogo_->add_named_object(no);
}

// Import a function into PACKAGE.  PACKAGE is normally
// THIS->PACKAGE_, but it will be different for a method associated
// with a type defined in a different package.

Named_object*
Import::import_func(Package* package)
{
  std::string name;
  Typed_identifier* receiver;
  Typed_identifier_list* parameters;
  Typed_identifier_list* results;
  bool is_varargs;
  Function::import_func(this, &name, &receiver, &parameters, &results,
			&is_varargs);
  Function_type *fntype = Type::make_function_type(receiver, parameters,
						   results, this->location_);
  if (is_varargs)
    fntype->set_is_varargs();

  Location loc = this->location_;
  Named_object* no;
  if (fntype->is_method())
    {
      Type* rtype = receiver->type();

      // We may still be reading the definition of RTYPE, so we have
      // to be careful to avoid calling base or convert.  If RTYPE is
      // a named type or a forward declaration, then we know that it
      // is not a pointer, because we are reading a method on RTYPE
      // and named pointers can't have methods.

      if (rtype->classification() == Type::TYPE_POINTER)
	rtype = rtype->points_to();

      if (rtype->is_error_type())
	return NULL;
      else if (rtype->named_type() != NULL)
	no = rtype->named_type()->add_method_declaration(name, package, fntype,
							 loc);
      else if (rtype->forward_declaration_type() != NULL)
	no = rtype->forward_declaration_type()->add_method_declaration(name,
								       package,
								       fntype,
								       loc);
      else
	go_unreachable();
    }
  else
    {
      no = package->add_function_declaration(name, fntype, loc);
      if (this->add_to_globals_)
	this->gogo_->add_named_object(no);
    }
  return no;
}

// Read a type in the import stream.  This records the type by the
// type index.  If the type is named, it registers the name, but marks
// it as invisible.

Type*
Import::read_type()
{
  Stream* stream = this->stream_;
  this->require_c_string("<type ");

  std::string number;
  int c;
  while (true)
    {
      c = stream->get_char();
      if (c != '-' && (c < '0' || c > '9'))
	break;
      number += c;
    }

  int index;
  if (!this->string_to_int(number, true, &index))
    return Type::make_error_type();

  if (c == '>')
    {
      // This type was already defined.
      if (index < 0
	  ? (static_cast<size_t>(- index) >= this->builtin_types_.size()
	     || this->builtin_types_[- index] == NULL)
	  : (static_cast<size_t>(index) >= this->types_.size()
	     || this->types_[index] == NULL))
	{
	  error_at(this->location_,
		   "error in import data at %d: bad type index %d",
		   stream->pos(), index);
	  stream->set_saw_error();
	  return Type::make_error_type();
	}

      return index < 0 ? this->builtin_types_[- index] : this->types_[index];
    }

  if (c != ' ')
    {
      if (!stream->saw_error())
	error_at(this->location_,
		 "error in import data at %d: expect %< %> or %<>%>'",
		 stream->pos());
      stream->set_saw_error();
      stream->advance(1);
      return Type::make_error_type();
    }

  if (index <= 0
      || (static_cast<size_t>(index) < this->types_.size()
	  && this->types_[index] != NULL))
    {
      error_at(this->location_,
	       "error in import data at %d: type index already defined",
	       stream->pos());
      stream->set_saw_error();
      return Type::make_error_type();
    }

  if (static_cast<size_t>(index) >= this->types_.size())
    {
      int newsize = std::max(static_cast<size_t>(index) + 1,
			     this->types_.size() * 2);
      this->types_.resize(newsize, NULL);
    }

  if (stream->peek_char() != '"')
    {
      Type* type = Type::import_type(this);
      this->require_c_string(">");
      this->types_[index] = type;
      return type;
    }

  // This type has a name.

  stream->advance(1);
  std::string type_name;
  while ((c = stream->get_char()) != '"')
    type_name += c;

  // If this type is in the current package, the name will be
  // .PREFIX.PACKAGE.NAME or simply NAME with no dots.  Otherwise, a
  // non-hidden symbol will be PREFIX.PACKAGE.NAME and a hidden symbol
  // will be .PREFIX.PACKAGE.NAME.
  std::string package_name;
  std::string unique_prefix;
  if (type_name.find('.') != std::string::npos)
    {
      bool is_hidden = false;
      size_t start = 0;
      if (type_name[0] == '.')
	{
	  ++start;
	  is_hidden = true;
	}
      size_t dot1 = type_name.find('.', start);
      size_t dot2;
      if (dot1 == std::string::npos)
	dot2 = std::string::npos;
      else
	dot2 = type_name.find('.', dot1 + 1);
      if (dot1 == std::string::npos || dot2 == std::string::npos)
	{
	  error_at(this->location_,
		   ("error at import data at %d: missing dot in type name"),
		   stream->pos());
	  stream->set_saw_error();
	}
      else
	{
	  unique_prefix = type_name.substr(start, dot1 - start);
	  package_name = type_name.substr(dot1 + 1, dot2 - (dot1 + 1));
	}
      if (!is_hidden)
	type_name.erase(0, dot2 + 1);
    }

  this->require_c_string(" ");

  // Declare the type in the appropriate package.  If we haven't seen
  // it before, mark it as invisible.  We declare it before we read
  // the actual definition of the type, since the definition may refer
  // to the type itself.
  Package* package;
  if (package_name.empty())
    package = this->package_;
  else
    package = this->gogo_->register_package(package_name, unique_prefix,
					    Linemap::unknown_location());

  Named_object* no = package->bindings()->lookup(type_name);
  if (no == NULL)
    no = package->add_type_declaration(type_name, this->location_);
  else if (!no->is_type_declaration() && !no->is_type())
    {
      error_at(this->location_, "imported %<%s.%s%> both type and non-type",
	       Gogo::message_name(package->name()).c_str(),
	       Gogo::message_name(type_name).c_str());
      stream->set_saw_error();
      return Type::make_error_type();
    }
  else
    go_assert(no->package() == package);

  if (this->types_[index] == NULL)
    {
      if (no->is_type_declaration())
	{
	  // FIXME: It's silly to make a forward declaration every time.
	  this->types_[index] = Type::make_forward_declaration(no);
	}
      else
	{
	  go_assert(no->is_type());
	  this->types_[index] = no->type_value();
	}
    }

  // If there is no type definition, then this is just a forward
  // declaration of a type defined in some other file.
  Type* type;
  if (this->match_c_string(">"))
    type = this->types_[index];
  else
    {
      type = this->read_type();

      if (no->is_type_declaration())
	{
	  // We can define the type now.

	  no = package->add_type(type_name, type, this->location_);
	  Named_type* ntype = no->type_value();

	  // This type has not yet been imported.
	  ntype->clear_is_visible();

	  type = ntype;
	}
      else if (no->is_type())
	{
	  // We have seen this type before.  FIXME: it would be a good
	  // idea to check that the two imported types are identical,
	  // but we have not finalized the methods yet, which means
	  // that we can not reliably compare interface types.
	  type = no->type_value();

	  // Don't change the visibility of the existing type.
	}

      this->types_[index] = type;

      // Read the type methods.
      if (this->match_c_string("\n"))
	{
	  this->advance(1);
	  while (this->match_c_string(" func"))
	    {
	      this->advance(1);
	      this->import_func(package);
	    }
	}
    }

  this->require_c_string(">");

  return type;
}

// Register the builtin types.

void
Import::register_builtin_types(Gogo* gogo)
{
  this->register_builtin_type(gogo, "int8", BUILTIN_INT8);
  this->register_builtin_type(gogo, "int16", BUILTIN_INT16);
  this->register_builtin_type(gogo, "int32", BUILTIN_INT32);
  this->register_builtin_type(gogo, "int64", BUILTIN_INT64);
  this->register_builtin_type(gogo, "uint8", BUILTIN_UINT8);
  this->register_builtin_type(gogo, "uint16", BUILTIN_UINT16);
  this->register_builtin_type(gogo, "uint32", BUILTIN_UINT32);
  this->register_builtin_type(gogo, "uint64", BUILTIN_UINT64);
  this->register_builtin_type(gogo, "float32", BUILTIN_FLOAT32);
  this->register_builtin_type(gogo, "float64", BUILTIN_FLOAT64);
  this->register_builtin_type(gogo, "complex64", BUILTIN_COMPLEX64);
  this->register_builtin_type(gogo, "complex128", BUILTIN_COMPLEX128);
  this->register_builtin_type(gogo, "int", BUILTIN_INT);
  this->register_builtin_type(gogo, "uint", BUILTIN_UINT);
  this->register_builtin_type(gogo, "uintptr", BUILTIN_UINTPTR);
  this->register_builtin_type(gogo, "bool", BUILTIN_BOOL);
  this->register_builtin_type(gogo, "string", BUILTIN_STRING);
  this->register_builtin_type(gogo, "error", BUILTIN_ERROR);
  this->register_builtin_type(gogo, "byte", BUILTIN_BYTE);
  this->register_builtin_type(gogo, "rune", BUILTIN_RUNE);
}

// Register a single builtin type.

void
Import::register_builtin_type(Gogo* gogo, const char* name, Builtin_code code)
{
  Named_object* named_object = gogo->lookup_global(name);
  go_assert(named_object != NULL && named_object->is_type());
  int index = - static_cast<int>(code);
  go_assert(index > 0
	     && static_cast<size_t>(index) < this->builtin_types_.size());
  this->builtin_types_[index] = named_object->type_value();
}

// Read an identifier from the stream.

std::string
Import::read_identifier()
{
  std::string ret;
  Stream* stream = this->stream_;
  int c;
  while (true)
    {
      c = stream->peek_char();
      if (c == -1 || c == ' ' || c == ';')
	break;
      ret += c;
      stream->advance(1);
    }
  return ret;
}

// Read a name from the stream.

std::string
Import::read_name()
{
  std::string ret = this->read_identifier();
  if (ret == "?")
    ret.clear();
  else if (!Lex::is_exported_name(ret))
    ret = ('.' + this->package_->unique_prefix()
	   + '.' + this->package_->name()
	   + '.' + ret);
  return ret;
}

// Turn a string into a integer with appropriate error handling.

bool
Import::string_to_int(const std::string &s, bool is_neg_ok, int* ret)
{
  char* end;
  long prio = strtol(s.c_str(), &end, 10);
  if (*end != '\0' || prio > 0x7fffffff || (prio < 0 && !is_neg_ok))
    {
      error_at(this->location_, "invalid integer in import data at %d",
	       this->stream_->pos());
      this->stream_->set_saw_error();
      return false;
    }
  *ret = prio;
  return true;
}

// Class Import::Stream.

Import::Stream::Stream()
  : pos_(0), saw_error_(false)
{
}

Import::Stream::~Stream()
{
}

// Return the next character to come from the stream.

int
Import::Stream::peek_char()
{
  const char* read;
  if (!this->do_peek(1, &read))
    return -1;
  // Make sure we return an unsigned char, so that we don't get
  // confused by \xff.
  unsigned char ret = *read;
  return ret;
}

// Return true if the next LENGTH characters from the stream match
// BYTES

bool
Import::Stream::match_bytes(const char* bytes, size_t length)
{
  const char* read;
  if (!this->do_peek(length, &read))
    return false;
  return memcmp(bytes, read, length) == 0;
}

// Require that the next LENGTH bytes from the stream match BYTES.

void
Import::Stream::require_bytes(Location location, const char* bytes,
			      size_t length)
{
  const char* read;
  if (!this->do_peek(length, &read)
      || memcmp(bytes, read, length) != 0)
    {
      if (!this->saw_error_)
	error_at(location, "import error at %d: expected %<%.*s%>",
		 this->pos(), static_cast<int>(length), bytes);
      this->saw_error_ = true;
      return;
    }
  this->advance(length);
}

// Class Stream_from_file.

Stream_from_file::Stream_from_file(int fd)
  : fd_(fd), data_()
{
  if (lseek(fd, 0, SEEK_SET) != 0)
    {
      error("lseek failed: %m");
      this->set_saw_error();
    }
}

Stream_from_file::~Stream_from_file()
{
  close(this->fd_);
}

// Read next bytes.

bool
Stream_from_file::do_peek(size_t length, const char** bytes)
{
  if (this->data_.length() <= length)
    {
      *bytes = this->data_.data();
      return true;
    }
  // Don't bother to handle the general case, since we don't need it.
  go_assert(length < 64);
  char buf[64];
  ssize_t got = read(this->fd_, buf, length);

  if (got < 0)
    {
      if (!this->saw_error())
	error("read failed: %m");
      this->set_saw_error();
      return false;
    }

  if (lseek(this->fd_, - got, SEEK_CUR) != 0)
    {
      if (!this->saw_error())
	error("lseek failed: %m");
      this->set_saw_error();
      return false;
    }

  if (static_cast<size_t>(got) < length)
    return false;

  this->data_.assign(buf, got);

  *bytes = this->data_.data();
  return true;
}

// Advance.

void
Stream_from_file::do_advance(size_t skip)
{
  if (lseek(this->fd_, skip, SEEK_CUR) != 0)
    {
      if (!this->saw_error())
	error("lseek failed: %m");
      this->set_saw_error();
    }
  if (!this->data_.empty())
    {
      if (this->data_.length() < skip)
	this->data_.erase(0, skip);
      else
	this->data_.clear();
    }
}
