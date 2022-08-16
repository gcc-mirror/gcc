/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse" } */

struct S
{
  long long s1 __attribute__ ((aligned (8)));
  unsigned s2, s3;
};

extern struct S foooo[];

void bar (int, int, int, int, int, int, struct S);

void
foo (void)
{
  bar (1, 2, 3, 4, 5, 6, foooo[0]);
}

/* { dg-final { scan-assembler "push\[lq\]\tfoooo\+" { target { nonpic || { ! ia32 } } } } }*/
/* { dg-final { scan-assembler "movl\tfoooo@GOT\\(%ebx\\), %eax" { target { ia32 && { ! nonpic } } } } } */
/* { dg-final { scan-assembler-times "pushl\t(?:|4|8|12)\\(%eax\\)" 4 { target { ia32 && { ! nonpic } } } } } */
