/* Copyright (C) 2002 Free Software Foundation, Inc.

   Source: Neil Booth, 12 Feb 2002.

   In the declaration of proc, x must be parsed as a typedef name (C99
   6.7.5.3 p11.  Also see C89 DR #009, which was erroneously omitted
   from C99, and resubmitted as DR #249: if in a parameter
   declaration, an identifier can be read as a typedef name or a
   parameter name, it is read as a typedef name).  */

/* { dg-do compile } */

typedef int x;
typedef int y;
int proc(int (x));	/* x is a typedef, param to proc is a function.  */
int proc2(int x);	/* x is an identifier, param is an int.  */

/* Parameter to proc3 is unnamed, with type a function that returns
   int and takes a single argument of type function with one int
   parameter returning int.  In particular, proc3 is not a function
   that takes a parameter y that is a function with one int parameter
   returning int.  8-)  */
int proc3(int (y (x)));

int main ()
{
  proc (proc2);		/* { dg-bogus "integer from pointer" } */
  return proc3 (proc);  /* { dg-bogus "incompatible pointer type" } */
}
