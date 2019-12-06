/* Test C2x UTF-8 characters.  Character values not affected by
   different execution character set.  */
/* { dg-do compile } */
/* { dg-require-iconv "IBM1047" } */
/* { dg-options "-std=c2x -pedantic-errors -fexec-charset=IBM1047" } */

_Static_assert (u8'a' == 97);
_Static_assert (u8'a' != (unsigned char) 'a');
