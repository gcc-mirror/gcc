/* Verify that offsetof warns if given a non-POD */
/* Copyright (C) 2003 Free Software Foundation, Inc. */
/* Contributed by Matt Austern <austern@apple.com> 15 May 2003 */
/* { dg-do compile } */

struct X
{
  X() : x(3), y(4) { }
  int x, y;
};

typedef X* pX;
typedef __SIZE_TYPE__ size_t;

size_t yoff = size_t(&(pX(0)->y)); /* { dg-warning "invalid access" "" } */
/* { dg-warning "macro was used incorrectly" "macro" { target *-*-* } 15 } */
