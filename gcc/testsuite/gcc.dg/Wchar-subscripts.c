/* Copyright (C) 2005  Free Software Foundation.

   by Gabriel Dos Reis  <gdr@integrable-solutions.net>  */

/* { dg-do compile }  */
/* { dg-options "-Wchar-subscripts" } */

int main(void)
{
  int ary[256] = { 0 };
  return ary['a'];
}
