/* Copyright (C) 2002 Free Software Foundation, Inc.

   Tests various diagnostics about a bit-field's type and width.

   Source: Neil Booth, 26 Jan 2002.
*/

/* { dg-options -pedantic } */

enum foo {e1 = 0, e2, e3, e4, e5};

int x;
typedef unsigned int ui;

struct bf1
{
  unsigned int a: 3.5;		/* { dg-error "integer constant" } */
  unsigned int b: x;		/* { dg-error "integer constant" } */
  unsigned int c: -1;		/* { dg-error "negative width" } */
  unsigned int d: 0;		/* { dg-error "zero width" } */
  unsigned int : 0;		/* { dg-bogus "zero width" } */
  unsigned int : 5;
  double e: 1;			/* { dg-error "invalid type" } */
  float f: 1;			/* { dg-error "invalid type" } */
  unsigned long g: 5;		/* { dg-warning "GCC extension|ISO C" } */
  ui h: 5;
  enum foo i: 2;		/* { dg-error "narrower" } */
  enum foo j: 3;
  unsigned int k: 256;		/* { dg-error "exceeds its type" } */
};
