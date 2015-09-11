/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mtune=corei7-avx -fdump-rtl-sched2" } */

int a[100];

double bar (double sum)
{
  int i = 100000;
  while (i != 0)
    {
      sum += (0.5 + (a[i%100] - 128));
      i--;
    }
  return sum;
}

/* { dg-final { scan-rtl-dump-not "compare.*insn.*jump_insn.*jump_insn" "sched2" } } */
