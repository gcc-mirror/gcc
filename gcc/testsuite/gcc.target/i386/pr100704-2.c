/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fomit-frame-pointer -march=x86-64" } */

struct S
{
  char array[64];
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
