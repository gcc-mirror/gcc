/* PR target/28911
   The following used to cause crash on m68k-elf because 0x80000000
   was used as an SImode constant.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -m68000" { target m68k-*-* } } */

_Complex float
foo (float a)
{
  return __builtin_copysign (a != a, a);
}
