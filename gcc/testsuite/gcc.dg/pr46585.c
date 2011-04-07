/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-fno-dce -fschedule-insns -fselective-scheduling" } */
void
foo (void)
{
  switch (0)
    {
    default:
      break;
    }
}
