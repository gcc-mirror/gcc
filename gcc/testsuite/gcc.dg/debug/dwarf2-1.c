/* { dg-do compile } */

/* Copyright (C) 2000  Free Software Foundation  */
/* Contributed by Alexandre Oliva <aoliva@cygnus.com> */

static int foo () { return 0; }

void bar () {
  int foo ();
  int foo ();
}
