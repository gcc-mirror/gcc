/* Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell 8 May 2001 <nathan@codesourcery.com> */

/* Test of prohibition on directives which result from macro
   expansion.  Same as direct2.c, with -save-temps applied; results
   should be identical.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -ansi -pedantic-errors" } */

#define HASH #
#define HASHDEFINE #define
#define HASHINCLUDE #include

HASH include "somerandomfile" /*{ dg-error "syntax|parse" "non-include" }*/
/*{ dg-bogus "No such" "don't execute non-include" { target *-*-* } 15 }*/
HASHINCLUDE <somerandomfile> /*{ dg-error "syntax|parse" "non-include 2" }*/
/*{ dg-bogus "No such" "don't execute non-include 2" { target *-*-* } 17 }*/

void g ()
{
HASH define X 1 /* { dg-error "syntax error" "# from macro" } */
HASHDEFINE  Y 1 /* { dg-error "syntax error" "#define from macro" } */
}

#pragma GCC dependency "direct2s.c"
#

void f ()
{
  int i = X;    /* { dg-error "undeclared|for each" "no macro X" } */
  int j = Y;    /* { dg-error "undeclared|for each" "no macro Y" } */
}
