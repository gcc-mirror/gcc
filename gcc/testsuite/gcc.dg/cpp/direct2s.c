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

HASH include "somerandomfile" /*{ dg-error "stray" "non-include" }*/
/*{ dg-bogus "No such" "don't execute non-include" { target *-*-* } 15 }*/
int resync_parser_1; /*{ dg-error "parse|syntax|expected" "" { target *-*-* } 15 }*/

HASHINCLUDE <somerandomfile> /*{ dg-error "stray|expected" "non-include 2" }*/
/*{ dg-bogus "No such" "don't execute non-include 2" { target *-*-* } 18 }*/
int resync_parser_2;

void g1 ()
{
HASH define X 1 /* { dg-error "stray|unknown|expected" "# from macro" } */
  int resync_parser_3;
}

void g2 ()
{
HASHDEFINE  Y 1 /* { dg-error "stray|unknown|expected" "#define from macro" } */
  int resync_parser_4;
}

#pragma GCC dependency "direct2.c"
#

void f ()
{
  int i = X;    /* { dg-error "undeclared|for each" "no macro X" } */
  int j = Y;    /* { dg-error "undeclared|for each" "no macro Y" } */
}

/* { dg-final { cleanup-saved-temps } } */
