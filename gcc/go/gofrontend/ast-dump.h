// ast-dump.h -- AST debug dump.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_AST_DUMP_H
#define GO_AST_DUMP_H

#include "string-dump.h"

class Expression;
class Expression_list;
class Named_object;
class Statement;
class Gogo;

// This class implements fgo-dump-ast. the
// Abstract syntax tree dump of the Go program.

class Ast_dump_context : public String_dump
{
 public:
  Ast_dump_context(std::ostream* out = NULL, bool dump_subblocks = true);

  // Initialize the dump context.
  void
  dump(Gogo*, const char* basename);

  // Dump spaces to dumpfile as indentation.
  void
  print_indent();

  // Increase current indentation for print_indent().
  void
  indent()
  { ++this->indent_;}

  // Decrease current indentation for print_indent().
  void
  unindent()
  { --this->indent_;}

  // Whether subblocks should be dumped or not.
  bool
  dump_subblocks()
  { return this->dump_subblocks_; }

  // Get dump output stream.
  std::ostream&
  ostream()
  { return *this->ostream_;}

  // Dump a Block to dump file.
  void
  dump_block(Block*);

  // Dump a type to dump file.
  void
  dump_type(const Type*);

  // Dump an expression to dump file.
  void
  dump_expression(const Expression*);

  // Dump an expression list to dump file.
  void
  dump_expression_list(const Expression_list*, bool as_pairs = false);

  // Dump a typed identifier to dump file.
  void
  dump_typed_identifier(const  Typed_identifier*);

  // Dump a typed identifier list to dump file.
  void
  dump_typed_identifier_list(const Typed_identifier_list*);

  // Dump temporary variable name to dump file.
  void
  dump_temp_variable_name(const Statement*);

  // Dump unamed lable name to dump file.
  void
  dump_label_name(const Unnamed_label*);

  // Dump operator symbol to dump file.
  void
  dump_operator(Operator);

  // Implementation of String_dump interface.
  void
  write_c_string(const char*);

  // Implements the String_dump interface.
  void
  write_string(const std::string& s);

  // Dump statement to stream.
  static void
  dump_to_stream(const Statement*, std::ostream*);

  // Dump expression to stream.
  static void
  dump_to_stream(const Expression* expr, std::ostream* out);

 private:
   // Current indent level.
  int indent_;

  // Indentation offset.
  static const int offset_;

  // Whether subblocks of composite statements should be dumped or not.
  bool dump_subblocks_;

  // Stream on output dump file.
  std::ostream* ostream_;

  Gogo* gogo_;
};

#endif  // GO_AST_DUMP_H
