/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(long long a)
{
   if (a & (long long) 0x400)
      return 1;
   return 0;
}

/* { dg-final { scan-assembler "andl" { target i?86-*-linux* x86_64-*-linux* } } } " */
/* { dg-final { scan-assembler-not "setne" { target i?86-*-linux* x86_64-*-linux* } } }" */
/* { dg-final { scan-assembler "and" { target arm*-*-* } } }" */
/* { dg-final { scan-assembler-not "moveq" { target arm*-*-* } } }" */
