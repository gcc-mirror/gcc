/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-fsigned-char" } */

/* Crosscompiling from i686-linux (32-bit) to x86_64-linux (64-bit)
   gave extra warnings on the two assignments:
   warning: large integer implicitly truncated to unsigned type
   warning: overflow in implicit constant conversion
   This test has been added as a regression test after fixing the bug
   by Andreas Jaeger, 23 Nov 2001.  */
int
main (void)
{
  signed char c = '\xff';
  unsigned char d = '\xff';
  
  return 0;
}
