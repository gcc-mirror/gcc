/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

/* Copyright (C) 2000  Free Software Foundation  */
/* Contributed by Alexandre Oliva <aoliva@cygnus.com> */

static int foo () {}

int bar () {
  int foo ();
  int foo ();
}
