/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* Test we output a warning for concatenation of artifical strings.

   Neil Booth, 10 Dec 2001.  */

void foo ()
{
  char str1[] = __FUNCTION__ ".";	/* { dg-warning "deprecated" } */
  char str2[] = __PRETTY_FUNCTION__ ".";/* { dg-warning "deprecated" } */
  char str3[] = "." __FUNCTION__;	/* { dg-warning "deprecated" } */
  char str4[] = "." __PRETTY_FUNCTION__;/* { dg-warning "deprecated" } */
  char str5[] = "." ".";	/* No warning.  */
}
