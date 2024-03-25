/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-w" } */

/* Source: Neil Booth, 24 Feb 2002.

   Test if compiler and preprocessor agree on signeness of wide
   chars.  */

int main ()
{
  __WCHAR_TYPE__ c = -1;

#if L'\x0' - 1 < 0
  if (c > 0)
    __builtin_abort ();
#else
  if (c < 0)
    __builtin_abort ();
#endif
 
  return 0;
}
