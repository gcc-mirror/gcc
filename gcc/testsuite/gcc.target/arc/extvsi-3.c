/* { dg-do compile } */
/* { dg-skip-if "avoid conflicts with -mcpu options" { *-*-* } { "-mcpu=*" } { "-mcpu=em" } } */
/* { dg-options "-O2 -mcpu=em" } */
struct S { int a : 5; };

/* An extra parameter is added to ensure a different register is used for input and output. */
int foo (int unused, struct S p)
{
  return p.a;
}

/* { dg-final { scan-assembler "and\\s+r0,r1,31" } } */
/* { dg-final { scan-assembler "xor\\s+r0,r0,16" } } */
/* { dg-final { scan-assembler "sub\\s+r0,r0,16" } } */
