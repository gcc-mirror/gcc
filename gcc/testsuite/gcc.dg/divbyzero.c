/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* Source: Neil Booth, Oct 22 2001.  PR 150 - warn about division by
   zero.  */

#define ZERO (4 - 6 + 2)

int main (int argc, char *argv[])
{
  int w = argc % ZERO;		/* { dg-warning "division by zero" } */
  int x = argc / 0;		/* { dg-warning "division by zero" } */
  int y = argc / ZERO;		/* { dg-warning "division by zero" } */

  double z = 0.0 / 0.0	;	/* { dg-bogus "division by zero" } */
  w = (ZERO ? y / ZERO : x);	/* { dg-bogus "division by zero" } */
  x = (ZERO ? argc % ZERO: x);  /* { dg-bogus "division by zero" } */

  return 0;
}
