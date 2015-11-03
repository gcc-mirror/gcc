/* Verify that we do not optimize away the frame header in foo when using
   -mframe-header-opt but are calling a weak function that may be overridden
   by a different function that does need the frame header.  Without the
   optimization foo should increment the stack by 24 bytes, with the
   optimization it would only be 8 bytes.  */

/* { dg-do compile } */
/* { dg-options "-mframe-header-opt -mabi=32 -mno-abicalls" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\taddiu\t\\\$sp,\\\$sp,-24" } } */

NOMIPS16 void __attribute__((noinline, weak))
bar (int* a)
{
  *a = 1;
}

void
NOMIPS16 foo (int a)
{
  bar (&a);
}
