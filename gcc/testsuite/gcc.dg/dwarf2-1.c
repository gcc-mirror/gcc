/* { dg-do compile } */
/* { dg-options "-O3 -gdwarf-2" } */

/* Copyright (C) 2000  Free Software Foundation  */
/* Contributed by Alexandre Oliva <aoliva@cygnus.com> */

static int foo () {}

int bar () {
  int foo ();
  int foo ();
}
