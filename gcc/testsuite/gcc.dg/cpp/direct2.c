/* Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell 8 May 2001 <nathan@codesourcery.com> */

/* Test of prohibition on directives which result from macro expansion.
   See also direct2s.c */

/* { dg-do compile } */

#define HASH #
#define HASHDEFINE #define
#define HASHINCLUDE #include

HASH include "somerandomfile" /*{ dg-error "syntax|parse" "non-include" }*/
/*{ dg-bogus "No such" "don't execute non-include" { target *-*-* } 13 }*/
HASHINCLUDE <somerandomfile> /*{ dg-error "syntax|parse" "non-include 2" }*/
/*{ dg-bogus "No such" "don't execute non-include 2" { target *-*-* } 15 }*/

void g ()
{
HASH define X 1 /* { dg-error "syntax error" "# from macro" } */
HASHDEFINE  Y 1 /* { dg-error "syntax error" "#define from macro" } */
}

#pragma GCC dependency "direct2.c"
#

void f ()
{
  int i = X;    /* { dg-error "undeclared|for each" "no macro X" } */
  int j = Y;    /* { dg-error "undeclared|for each" "no macro Y" } */
}

#define slashstar /##*
#define starslash *##/

slashstar starslash /* { dg-error "(parse|syntax) error" "not a comment" } */
/* { dg-warning "does not give" "paste warning(s)" { target *-*-* } 36 } */
