/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* Test we output an error for concatenation of artificial strings.

   Neil Booth, 10 Dec 2001.  */

void foo ()
{
  char s1[] = __FUNCTION__".";	     /* { dg-error "(parse|syntax|expected|invalid|array)" } */
  char s2[] = __PRETTY_FUNCTION__".";/* { dg-error "(parse|syntax|expected|invalid|array)" } */
  char s3[] = "."__FUNCTION__;	     /* { dg-error "(parse|syntax|expected|invalid)" } */
  char s4[] = "."__PRETTY_FUNCTION__;/* { dg-error "(parse|syntax|expected|invalid)" } */
  char s5[] = "."".";                /* No error.  */
}
