// { dg-do compile }

// Copyright (C) 2005 Free Software Foundation, Inc.

// PR c++/24907 [3.4/4.0/4.1/4.2 Regression] "int x, ;" accepted

int x;
int y,; /* { dg-error "expected" } */

int main()
{
  int a;
  int b,;    /* { dg-error "expected" } */
  int c,d;
  int e,f,;  /* { dg-error "expected" } */
  int g,h,i;
  int j,k,l,;/* { dg-error "expected" } */
  int m,,,n; /* { dg-error "expected" } */
}
