// operator.h -- Go frontend operators.     -*- C++ -*-

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef GO_OPERATOR_H
#define GO_OPERATOR_H

// The operators.

enum Operator
{
  OPERATOR_INVALID,
  OPERATOR_OROR,	// ||
  OPERATOR_ANDAND,	// &&
  OPERATOR_EQEQ,	// ==
  OPERATOR_NOTEQ,	// !=
  OPERATOR_LT,		// <
  OPERATOR_LE,		// <=
  OPERATOR_GT,		// >
  OPERATOR_GE,		// >=
  OPERATOR_PLUS,	// +
  OPERATOR_MINUS,	// -
  OPERATOR_OR,		// |
  OPERATOR_XOR,		// ^
  OPERATOR_MULT,	// *
  OPERATOR_DIV,		// /
  OPERATOR_MOD,		// %
  OPERATOR_LSHIFT,	// <<
  OPERATOR_RSHIFT,	// >>
  OPERATOR_AND,		// &
  OPERATOR_NOT,		// !
  OPERATOR_BITCLEAR,	// &^
  OPERATOR_CHANOP,	// <-

  OPERATOR_EQ,		// =
  OPERATOR_PLUSEQ,	// +=
  OPERATOR_MINUSEQ,	// -=
  OPERATOR_OREQ,	// |=
  OPERATOR_XOREQ,	// ^=
  OPERATOR_MULTEQ,	// *=
  OPERATOR_DIVEQ,	// /=
  OPERATOR_MODEQ,	// %=
  OPERATOR_LSHIFTEQ,	// <<=
  OPERATOR_RSHIFTEQ,	// >>=
  OPERATOR_ANDEQ,	// &=
  OPERATOR_BITCLEAREQ,	// &^=
  OPERATOR_PLUSPLUS,	// ++
  OPERATOR_MINUSMINUS,	// --

  OPERATOR_COLON,	// :
  OPERATOR_COLONEQ,	// :=
  OPERATOR_SEMICOLON,	// ;
  OPERATOR_DOT,		// .
  OPERATOR_ELLIPSIS,	// ...
  OPERATOR_COMMA,	// ,
  OPERATOR_LPAREN,	// (
  OPERATOR_RPAREN,	// )
  OPERATOR_LCURLY,	// {
  OPERATOR_RCURLY,	// }
  OPERATOR_LSQUARE,	// [
  OPERATOR_RSQUARE	// ]
};

#endif // !defined(GO_OPERATOR_H)
