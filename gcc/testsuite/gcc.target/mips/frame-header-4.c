/* Verify that we can optimize away the frame header allocation in bar
   by having it use its frame header to store $31 in before calling foo.  */

/* { dg-do compile } */
/* { dg-options "-mframe-header-opt -mabi=32" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\taddiu\t\\\$sp" } } */

int __attribute__ ((noinline))
foo (int a, int b)
{
  return a + b;
}

int  __attribute__ ((noinline))
bar (int a, int b)
{
  return 1 + foo (a,b);
}

