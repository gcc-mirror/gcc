// ast-dump.h -- AST debug dump.    -*- C++ -*-

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_AST_DUMP_H
#define GO_AST_DUMP_H

class Expression;
class Expression_list;
class Named_object;
class Statement;
class Gogo;

// This class implements fgo-dump-ast. the
// Abstract syntax tree dump of the Go program.

class Ast_dump_context 
{
 public:
  Ast_dump_context();

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
  dump_expression_list(const Expression_list*);
  
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
    
 private:
   // Current indent level.
  int indent_;
  
  // Indentation offset.
  static const int offset_;
  
  // Stream on output dump file.
  std::ostream* ostream_;
    
  Gogo* gogo_;
};

#endif  // GO_AST_DUMP_H
