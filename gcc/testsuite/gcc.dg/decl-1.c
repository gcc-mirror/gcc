/* Copyright (C) 2002 Free Software Foundation, Inc.

   Source: Neil Booth, 12 Feb 2002.

   In the declaration of proc, x must be parsed as a typedef name
   (6.7.5.3 p11).  */

/* { dg-do compile } */

typedef int x;
int proc(int (x));	/* x is a typedef, param to proc is a function.  */
int proc2(int x);	/* x is an identifier, param is an int.  */

int main ()
{
  return proc (proc2);		/* { dg-bogus "integer from pointer" } */
}
