/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mtune=corei7 -fdump-rtl-sched2" } */

int a[100];

double bar (double sum)
{
  int i;
  for (i = 0; i < 1000000; i++)
   sum += (0.5 + (a[i%100] - 128));
  return sum;
}

/* { dg-final { scan-rtl-dump-not "compare.*insn.*jump_insn.*jump_insn" "sched2" } } */
