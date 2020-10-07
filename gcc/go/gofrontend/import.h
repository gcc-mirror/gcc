// import.h -- Go frontend import declarations.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_IMPORT_H
#define GO_IMPORT_H

#include "export.h"
#include "go-linemap.h"

class Gogo;
class Block;
class Package;
class Type;
class Named_object;
class Named_type;
class Expression;
class Import_function_body;
class Temporary_statement;
class Unnamed_label;
class Finalize_methods;

// Expressions can be imported either directly from import data (for
// simple constant expressions that can appear in a const declaration
// or as an array length in a type definition) or from an exported
// function body (for an inlinable function).  These two cases happen
// at different points in the compilation and have different
// requirements, so it's not easy to unify them.  Import_expression is
// an abstract interface that permits the expression import code to
// work at either point.  When importing expressions that only occur
// for an inlinable function, the ifb method is available to get the
// full Import_function_body.

class Import_expression
{
 public:
  // Return the import function body.  This should only be called for
  // expressions that can not appear outside of an inlinable function
  // body.
  virtual Import_function_body*
  ifb() = 0;

  // The location to report in an error message.
  virtual Location
  location() const = 0;

  // Peek at the next character in the input, returning a value from 0
  // to 0xff.  Returns -1 at end of stream.
  virtual int
  peek_char() = 0;

  // Return the next character and advance.
  virtual int
  get_char() = 0;

  // Return true if the next bytes match STR.
  virtual bool
  match_c_string(const char* str) = 0;

  // Require that the next bytes match STR.
  virtual void
  require_c_string(const char* str) = 0;

  // Advance the stream SKIP bytes.
  virtual void
  advance(size_t skip) = 0;

  // Read an identifier.
  virtual std::string
  read_identifier() = 0;

  // Read a type.
  virtual Type*
  read_type() = 0;

  // Return the maximum valid package index.
  virtual size_t
  max_package_index() const = 0;

  // Return the package for a package index.
  virtual Package*
  package_at_index(int index) = 0;

  // Return the version number of the export data we're reading.
  virtual Export_data_version
  version() const = 0;
};

// This class manages importing Go declarations.

class Import : public Import_expression
{
 public:
  // The Stream class is an interface used to read the data.  The
  // caller should instantiate a child of this class.
  class Stream
  {
   public:
    Stream();
    virtual ~Stream();

    // Set the position, for error messages.
    void
    set_pos(int pos)
    { this->pos_ = pos; }

    // Return whether we have seen an error.
    bool
    saw_error() const
    { return this->saw_error_; }

    // Record that we've seen an error.
    void
    set_saw_error()
    { this->saw_error_ = true; }

    // Return the next character (a value from 0 to 0xff) without
    // advancing.  Returns -1 at end of stream.
    int
    peek_char();

    // Look for LENGTH characters, setting *BYTES to point to them.
    // Returns false if the bytes are not available.  Does not
    // advance.
    bool
    peek(size_t length, const char** bytes)
    { return this->do_peek(length, bytes); }

    // Return the next character (a value from 0 to 0xff) and advance
    // the read position by 1.  Returns -1 at end of stream.
    int
    get_char()
    {
      int c = this->peek_char();
      this->advance(1);
      return c;
    }

    // Return true if at the end of the stream.
    bool
    at_eof()
    { return this->peek_char() == -1; }

    // Return true if the next bytes match STR.
    bool
    match_c_string(const char* str)
    { return this->match_bytes(str, strlen(str)); }

    // Return true if the next LENGTH bytes match BYTES.
    bool
    match_bytes(const char* bytes, size_t length);

    // Give an error if the next bytes do not match STR.  Advance the
    // read position by the length of STR.
    void
    require_c_string(Location location, const char* str)
    { this->require_bytes(location, str, strlen(str)); }

    // Given an error if the next LENGTH bytes do not match BYTES.
    // Advance the read position by LENGTH.
    void
    require_bytes(Location, const char* bytes, size_t length);

    // Advance the read position by SKIP bytes.
    void
    advance(size_t skip)
    {
      this->do_advance(skip);
      this->pos_ += skip;
    }

    // Return the current read position.  This returns int because it
    // is more convenient in error reporting.  FIXME.
    int
    pos()
    { return static_cast<int>(this->pos_); }

   protected:
    // This function should set *BYTES to point to a buffer holding
    // the LENGTH bytes at the current read position.  It should
    // return false if the bytes are not available.  This should not
    // change the current read position.
    virtual bool
    do_peek(size_t length, const char** bytes) = 0;

    // This function should advance the current read position LENGTH
    // bytes.
    virtual void
    do_advance(size_t skip) = 0;

   private:
    // The current read position.
    size_t pos_;
    // True if we've seen an error reading from this stream.
    bool saw_error_;
  };

  // Find import data.  This searches the file system for FILENAME and
  // returns a pointer to a Stream object to read the data that it
  // exports.  LOCATION is the location of the import statement.
  // RELATIVE_IMPORT_PATH is used as a prefix for a relative import.
  static Stream*
  open_package(const std::string& filename, Location location,
	       const std::string& relative_import_path);

  // Constructor.
  Import(Stream*, Location);

  // Register the builtin types.
  void
  register_builtin_types(Gogo*);

  // Import everything defined in the stream.  LOCAL_NAME is the local
  // name to be used for bindings; if it is the string "." then
  // bindings should be inserted in the global scope.  If LOCAL_NAME
  // is the empty string then the name of the package itself is the
  // local name.  This returns the imported package, or NULL on error.
  Package*
  import(Gogo*, const std::string& local_name, bool is_local_name_exported);

  // The location of the import statement.
  Location
  location() const
  { return this->location_; }

  // Return the package we are importing.
  Package*
  package() const
  { return this->package_; }

  // Return the next character.
  int
  peek_char()
  { return this->stream_->peek_char(); }

  // Return the next character and advance.
  int
  get_char()
  { return this->stream_->get_char(); }

  // Read LENGTH characters into *OUT and advance past them.  On
  // EOF reports an error and sets *OUT to an empty string.
  void
  read(size_t length, std::string* out);

  // Return true at the end of the stream.
  bool
  at_eof()
  { return this->stream_->at_eof(); }

  // Return whether the next bytes match STR.
  bool
  match_c_string(const char* str)
  { return this->stream_->match_c_string(str); }

  // Require that the next bytes match STR.
  void
  require_c_string(const char* str)
  { this->stream_->require_c_string(this->location_, str); }

  // Advance the stream SKIP bytes.
  void
  advance(size_t skip)
  { this->stream_->advance(skip); }

  // Stream position, for error reporting.
  int
  pos()
  { return this->stream_->pos(); }

  // Return the version number of the export data we're reading.
  Export_data_version
  version() const { return this->version_; }

  // Skip a semicolon if using an older version.
  void
  require_semicolon_if_old_version()
  {
    if (this->version_ == EXPORT_FORMAT_V1
	|| this->version_ == EXPORT_FORMAT_V2)
      this->require_c_string(";");
  }

  // Read an identifier.
  std::string
  read_identifier();

  // Read a name.  This is like read_identifier, except that a "?" is
  // returned as an empty string.  This matches Export::write_name.
  std::string
  read_name();

  // Return the maximum valid package index.  This is the size of
  // packages_ because we will subtract 1 in package_at_index.
  size_t
  max_package_index() const
  { return this->packages_.size(); }

  // Return the package at an index.  (We subtract 1 because package
  // index 0 is not used.)
  Package*
  package_at_index(int index)
  { return this->packages_.at(index - 1); }

  // Read a type.
  Type*
  read_type();

  // Return the type for a type index.  INPUT_NAME and INPUT_OFFSET
  // are only for error reporting.  PARSED is set to whether we parsed
  // the type information for a new type.
  Type*
  type_for_index(int index, const std::string& input_name,
		 size_t input_offset, bool* parsed);

  // Read an escape note.
  std::string
  read_escape();

  // Clear the stream when it is no longer accessible.
  void
  clear_stream()
  { this->stream_ = NULL; }

  // Just so that Import implements Import_expression.
  Import_function_body*
  ifb()
  { return NULL; }

  // Read a qualified identifier from an Import_expression.  Sets
  // *NAME, *PKG, and *IS_EXPORTED, and reports whether it succeeded.
  static bool
  read_qualified_identifier(Import_expression*, std::string* name,
			    Package** pkg, bool* is_exported);

 private:
  static Stream*
  try_package_in_directory(const std::string&, Location);

  static int
  try_suffixes(std::string*);

  static Stream*
  find_export_data(const std::string& filename, int fd, Location);

  static Stream*
  find_object_export_data(const std::string& filename, int fd,
			  off_t offset, Location);

  static const int archive_magic_len = 8;

  static bool
  is_archive_magic(const char*);

  static Stream*
  find_archive_export_data(const std::string& filename, int fd,
			   Location);

  // Read a package line.
  void
  read_one_package();

  // Read an import line.
  void
  read_one_import();

  // Read an indirectimport line.
  void
  read_one_indirect_import();

  // Read the import control functions and init graph.
  void
  read_import_init_fns(Gogo*);

  // Read the types.
  bool
  read_types();

  // Import a constant.
  void
  import_const();

  // Import a type.
  void
  import_type();

  // Import a variable.
  void
  import_var();

  // Import a function.
  void
  import_func(Package*);

  // Parse a type definition.
  bool
  parse_type(int index);

  // Read a named type and store it at this->type_[index].
  Type*
  read_named_type(int index);

  // Register a single builtin type.
  void
  register_builtin_type(Gogo*, const char* name, Builtin_code);

  // Get an integer from a string.
  bool
  string_to_int(const std::string&, bool is_neg_ok, int* ret);

  // Get an unsigned integer from a string.
  bool
  string_to_unsigned(const std::string& s, unsigned* ret)
  {
    int ivalue;
    if (!this->string_to_int(s, false, &ivalue))
      return false;
    *ret = static_cast<unsigned>(ivalue);
    return true;
  }

  // Finalize methods for newly imported types.
  void
  finalize_methods();

  // The general IR.
  Gogo* gogo_;
  // The stream from which to read import data.
  Stream* stream_;
  // The location of the import statement we are processing.
  Location location_;
  // The package we are importing.
  Package* package_;
  // Whether to add new objects to the global scope, rather than to a
  // package scope.
  bool add_to_globals_;
  // Mapping from package index to package.
  std::vector<Package*> packages_;
  // All type data.
  std::string type_data_;
  // Position of type data in the stream.
  int type_pos_;
  // Mapping from type code to offset/length in type_data_.
  std::vector<std::pair<size_t, size_t> > type_offsets_;
  // Mapping from negated builtin type codes to Type structures.
  std::vector<Named_type*> builtin_types_;
  // Mapping from exported type codes to Type structures.
  std::vector<Type*> types_;
  // Version of export data we're reading.
  Export_data_version version_;
};

// Read import data from a string.

class Stream_from_string : public Import::Stream
{
 public:
  Stream_from_string(const std::string& str)
    : str_(str), pos_(0)
  { }

 protected:
  bool
  do_peek(size_t length, const char** bytes)
  {
    if (this->pos_ + length > this->str_.length())
      return false;
    *bytes = this->str_.data() + this->pos_;
    return true;
  }

  void
  do_advance(size_t len)
  { this->pos_ += len; }

 private:
  // The string of data we are reading.
  std::string str_;
  // The current position within the string.
  size_t pos_;
};

// Read import data from a buffer allocated using malloc.

class Stream_from_buffer : public Import::Stream
{
 public:
  Stream_from_buffer(char* buf, size_t length)
    : buf_(buf), length_(length), pos_(0)
  { }

  ~Stream_from_buffer()
  { free(this->buf_); }

 protected:
  bool
  do_peek(size_t length, const char** bytes)
  {
    if (this->pos_ + length > this->length_)
      return false;
    *bytes = this->buf_ + this->pos_;
    return true;
  }

  void
  do_advance(size_t len)
  { this->pos_ += len; }

 private:
  // The data we are reading.
  char* buf_;
  // The length of the buffer.
  size_t length_;
  // The current position within the buffer.
  size_t pos_;
};

// Read import data from an open file descriptor.

class Stream_from_file : public Import::Stream
{
 public:
  Stream_from_file(int fd);

  ~Stream_from_file();

 protected:
  bool
  do_peek(size_t, const char**);

  void
  do_advance(size_t);

 private:
  // No copying.
  Stream_from_file(const Stream_from_file&);
  Stream_from_file& operator=(const Stream_from_file&);

  // The file descriptor.
  int fd_;
  // Data read from the file.
  std::string data_;
};

// Read import data from an offset into a std::string.  This uses a
// reference to the string, to avoid copying, so the string must be
// kept alive through some other mechanism.

class Stream_from_string_ref : public Import::Stream
{
 public:
  Stream_from_string_ref(const std::string& str, size_t offset, size_t length)
    : str_(str), pos_(offset), end_(offset + length)
  { }

  ~Stream_from_string_ref()
  {}

 protected:
  bool
  do_peek(size_t length, const char** bytes)
  {
    if (this->pos_ + length > this->end_)
      return false;
    *bytes = &this->str_[this->pos_];
    return true;
  }

  void
  do_advance(size_t length)
  { this->pos_ += length; }

 private:
  // A reference to the string we are reading from.
  const std::string& str_;
  // The current offset into the string.
  size_t pos_;
  // The index after the last byte we can read.
  size_t end_;
};

// Class to manage importing a function body.  This is passed around
// to Statements and Expressions.  It parses the function into the IR.

class Import_function_body : public Import_expression
{
 public:
  Import_function_body(Gogo* gogo, Import* imp, Named_object* named_object,
		       const std::string& body, size_t off, Block* block,
		       int indent);
  ~Import_function_body();

  // The IR.
  Gogo*
  gogo()
  { return this->gogo_; }

  // The location to report in an error message.
  Location
  location() const
  { return this->imp_->location(); }

  // The function we are importing.
  Named_object*
  function() const
  { return this->named_object_; }

  // A reference to the body we are reading.
  const std::string&
  body() const
  { return this->body_; }

  // The current offset into the body.
  size_t
  off()
  { return this->off_; }

  // Update the offset into the body.
  void
  set_off(size_t off)
  { this->off_ = off; }

  // Advance the offset by SKIP bytes.
  void
  advance(size_t skip)
  { this->off_ += skip; }

  // The current block.
  Block*
  block()
  { return this->blocks_.back(); }

  // Begin importing a new block BLOCK nested within the current block.
  void
  begin_block(Block *block)
  { this->blocks_.push_back(block); }

  // Record the fact that we're done importing the current block.
  void
  finish_block()
  { this->blocks_.pop_back(); }

  // The current indentation.
  int
  indent() const
  { return this->indent_; }

  // Increment the indentation level.
  void
  increment_indent()
  { ++this->indent_; }

  // Decrement the indentation level.
  void
  decrement_indent()
  { --this->indent_; }

  // The name of the function we are parsing.
  const std::string&
  name() const;

  // Return the next character in the input stream, or -1 at the end.
  int
  peek_char()
  {
    if (this->body_.length() <= this->off_)
      return -1;
    return static_cast<unsigned char>(this->body_[this->off_]);
  }

  // Return the next character and advance.
  int
  get_char()
  {
    if (this->body_.length() <= this->off_)
      return -1;
    int c = static_cast<unsigned char>(this->body_[this->off_]);
    this->off_++;
    return c;
  }

  // Return whether the C string matches the current body position.
  bool
  match_c_string(const char* str)
  {
    size_t len = strlen(str);
    return (this->body_.length() >= this->off_ + len
	    && this->body_.compare(this->off_, len, str) == 0);
  }

  // Give an error if the next bytes do not match STR.  Advance the
  // offset by the length of STR.
  void
  require_c_string(const char* str);

  // Read an identifier.
  std::string
  read_identifier();

  // Read a type.
  Type*
  read_type();

  Export_data_version
  version() const
  { return this->imp_->version(); }

  // Record the index of a temporary statement.
  void
  record_temporary(Temporary_statement*, unsigned int);

  // Return a temporary statement given an index.
  Temporary_statement*
  temporary_statement(unsigned int);

  // Return an unnamed label given an index, defining the label if we
  // haven't seen it already.
  Unnamed_label*
  unnamed_label(unsigned int, Location);

  // Implement Import_expression.
  Import_function_body*
  ifb()
  { return this; }

  // Return the maximum valid package index.
  size_t
  max_package_index() const
  { return this->imp_->max_package_index(); }

  // Return the package at an index.
  Package*
  package_at_index(int index)
  { return this->imp_->package_at_index(index); }

  // Return whether we have seen an error.
  bool
  saw_error() const
  { return this->saw_error_; }

  // Record that we have seen an error.
  void
  set_saw_error()
  { this->saw_error_ = true; }

 private:
  static size_t
  next_size(size_t);

  // The IR.
  Gogo* gogo_;
  // The importer.
  Import* imp_;
  // The function we are parsing.
  Named_object* named_object_;
  // The exported data we are parsing.  Note that this is a reference;
  // the body string must laster longer than this object.
  const std::string& body_;
  // The current offset into body_.
  size_t off_;
  // Stack to record nesting of blocks being imported.
  std::vector<Block *> blocks_;
  // Current expected indentation level.
  int indent_;
  // Temporary statements by index.
  std::vector<Temporary_statement*> temporaries_;
  // Unnamed labels by index.
  std::vector<Unnamed_label*> labels_;
  // Whether we've seen an error.  Used to avoid reporting excess
  // errors.
  bool saw_error_;
};

#endif // !defined(GO_IMPORT_H)
