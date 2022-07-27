/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Os" } */

int foo(char x)
{
  return (x & 123) != 0;
}

/* { dg-final { scan-assembler-not "%dil" } } */
