/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do compile } */

#define plus +

void foo()
{
  int a, b = 1;

  /* The correct "a = 1 + ++b" will compile.
     The incorrect "a = 1 +++b" won't.  */
  a = 1 plus++b;
}
