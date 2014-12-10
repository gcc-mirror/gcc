/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fprofile-generate -fgcse -fno-gcse-lm -fgcse-sm -fno-ivopts -fno-tree-loop-im -ftree-pre -funroll-loops -fno-web -fschedule-insns2 -fselective-scheduling2 -fsel-sched-pipelining" } */

extern volatile float f[];

void foo (void)
{
  int i;
  for (i = 0; i < 100; i++)
    f[i] = 0;
  for (i = 0; i < 100; i++)
    f[i] = 0;
  for (i = 0; i < 100; i++)
    if (f[i])
      __builtin_abort ();
}
