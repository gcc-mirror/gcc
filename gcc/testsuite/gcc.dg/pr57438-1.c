/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-mdynamic-no-pic" { target powerpc*-*-darwin* } }

/* This is testing that a completely empty function body results in the
   insertion of a ud2/trap instruction to prevent a zero-sized FDE, and/or
   the function label apparently pointing to following code.  */

__attribute__((noinline))
void foo (void)
{
  __builtin_unreachable();
}

/* { dg-final { scan-assembler "ud2" { target  { i?86-*-darwin*  x86_64-*-darwin* } } } } */
/* { dg-final { scan-assembler "trap" { target { powerpc*-*-darwin* } } } } */
