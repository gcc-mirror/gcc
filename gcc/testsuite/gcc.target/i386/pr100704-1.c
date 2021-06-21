/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64" } */

struct S
{
  long long s1 __attribute__ ((aligned (8)));
  unsigned s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14;
};

extern struct S a[];

void bar (struct S);

void
foo (void)
{
  bar (a[0]);
}

/* { dg-final { scan-assembler-not "pushq" } } */
/* { dg-final { scan-assembler-times "movups\[\\t \]%xmm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movups\[\\t \]%xmm\[0-9\]+, 16\\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movups\[\\t \]%xmm\[0-9\]+, 32\\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "movups\[\\t \]%xmm\[0-9\]+, 48\\(%\[\^,\]+\\)" 1 } } */
