/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* Source: Neil Booth, 15 Sep 2001.

   A silly test to check that if a function-like macro name is
   immediately followed by a directive, then we process the directive
   properly.  */

#define main()
int main
#define mainbody () { return 0; }
mainbody
