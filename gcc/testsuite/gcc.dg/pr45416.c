/* { dg-do compile } */
/* { dg-skip-if "Skip for Thumb1." { { arm*-*-* } && { arm_thumb1_ok } } { "*" } { "" } } */
/* { dg-options "-O2" } */

int foo(long long a)
{
   if (a & (long long) 0x400)
      return 1;
   return 0;
}

/* { dg-final { scan-assembler "andl" { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } } " */
/* { dg-final { scan-assembler-not "setne" { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } }" */
/* { dg-final { scan-assembler "and|ubfx"  { target arm*-*-* } } } */
/* { dg-final { scan-assembler-not "moveq" { target arm*-*-* } } }" */
