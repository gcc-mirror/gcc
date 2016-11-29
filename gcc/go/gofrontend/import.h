// import.h -- Go frontend import declarations.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_IMPORT_H
#define GO_IMPORT_H

#include "export.h"
#include "go-linemap.h"

class Gogo;
class Package;
class Type;
class Named_object;
class Named_type;
class Expression;

// This class manages importing Go declarations.

class Import
{
 public:
  // The Stream class is an interface used to read the data.  The
  // caller should instantiate a child of this class.
  class Stream
  {
   public:
    Stream();
    virtual ~Stream();

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

  // Read an identifier.
  std::string
  read_identifier();

  // Read a name.  This is like read_identifier, except that a "?" is
  // returned as an empty string.  This matches Export::write_name.
  std::string
  read_name();

  // Read a type.
  Type*
  read_type();

  // Read an escape note.
  std::string
  read_escape();

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

  // Read the import control functions and init graph.
  void
  read_import_init_fns(Gogo*);

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
  Named_object*
  import_func(Package*);

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

  // Return the version number of the export data we're reading.
  Export_data_version
  version() const { return this->version_; }

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

#endif // !defined(GO_IMPORT_H)
