/* { dg-do-run } */
/* { dg-require-effective-target word_mode_no_slow_unalign } */
/* { dg-additional-options "-fdump-rtl-final" } */

/* Copyright 1996, 1999, 2007 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   Please email any bugs, comments, and/or additions to this file to:
   bug-gdb@prep.ai.mit.edu  */

#include <stdio.h>

struct struct3 { char a, b, c; };
struct struct3 foo3 = { 'A', 'B', 'C'},  L3;

struct struct3  fun3()
{
  return foo3;
}

#ifdef PROTOTYPES
void Fun3(struct struct3 foo3)
#else
void Fun3(foo3)
     struct struct3 foo3;
#endif
{
  L3 = foo3;
}

int main()
{
  struct struct3 x = fun3();

  printf("a:%c, b:%c, c:%c\n", x.a, x.b, x.c);
}

/* { dg-final { scan-rtl-dump-not {zero_extract:.+\[\s*foo3\s*\]} "final" } } */

