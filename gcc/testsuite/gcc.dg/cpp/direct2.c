/* Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell 8 May 2001 <nathan@codesourcery.com> */

/* Test of prohibition on directives which result from macro expansion.
   See also direct2s.c */

/* { dg-do compile } */

#define HASH #
#define HASHDEFINE #define
#define HASHINCLUDE #include

HASH include "somerandomfile" /*{ dg-error "stray" "non-include" }*/
/*{ dg-bogus "No such" "don't execute non-include" { target *-*-* } 13 }*/
int resync_parser_1; /*{ dg-error "parse|syntax|expected" "" }*/

HASHINCLUDE <somerandomfile> /*{ dg-error "stray|expected" "non-include 2" }*/
/*{ dg-bogus "No such" "don't execute non-include 2" { target *-*-* } 17 }*/
int resync_parser_2;

void g1 ()
{
HASH define X 1 /* { dg-error "stray|undeclared|parse|syntax|expected|for each" "# from macro" } */
  int resync_parser_3;
}

void g2 ()
{
HASHDEFINE  Y 1 /* { dg-error "stray|undeclared|parse|syntax|expected|for each" "#define from macro" } */
  int resync_parser_4;
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

slashstar starslash /* { dg-error "parse error|syntax error|expected" "not a comment" } */
/* { dg-warning "does not give" "paste warning(s)" { target *-*-* } 45 } */
