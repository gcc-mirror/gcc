/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-fshort-wchar" } */
/* { dg-options "-fshort-wchar -Wl,--no-wchar-size-warning" { target arm*-*-*eabi* } } */

/* Source: Neil Booth, 10 Dec 2002.

   Test that __WCHAR_MAX__ is correct with -fshort-wchar.  */

extern void abort (void);

int main ()
{
  __WCHAR_TYPE__ w = ~(__WCHAR_TYPE__) 0;

  if (w != __WCHAR_MAX__)
    abort ();
 
  return 0;
}
