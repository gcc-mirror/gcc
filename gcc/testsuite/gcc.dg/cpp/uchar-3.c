/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-funsigned-char -fpreprocessed" } */

/* Source: Ziemowit Laski.  -fpreprocessed doesn't define macros, but
   CPP would interpret charconsts based upon whether __CHAR_UNSIGNED__
   was defined.  */

int foo()
{
  char f = 0x83;
  if (f == '\x83')		/* { dg-bogus "always false" } */
    f = 0;

  return 0;
}
