/* Verify that -Wno-invalid-offsetof disables warning */
/* Copyright (C) 2003 Free Software Foundation, Inc. */
/* Contributed by Matt Austern <austern@apple.com> 15 May 2003 */
/* { dg-do compile } */
/* { dg-options "-Wno-invalid-offsetof" } */

struct X
{
  X() : x(3), y(4) { }
  int x, y;
};

typedef X* pX;
typedef __SIZE_TYPE__ size_t;

size_t yoff = size_t(&(pX(0)->y));
