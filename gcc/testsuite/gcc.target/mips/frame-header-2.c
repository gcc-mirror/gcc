/* Verify that we do optimize away the frame header in foo when using
   -mframe-header-opt by checking the stack pointer increment done in
   that function.  Without the optimization foo should increment the
   stack by 24 bytes, with the optimization it would only be 8 bytes.  */

/* { dg-do compile } */
/* { dg-options "-mframe-header-opt -mabi=32 -mno-abicalls" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\taddiu\t\\\$sp,\\\$sp,-8" } } */

NOMIPS16 void __attribute__((noinline)) __attribute__((noipa))
bar (int* a)
{
  *a = 1;
}

NOMIPS16 void
foo (int a)
{
  bar (&a);
}
