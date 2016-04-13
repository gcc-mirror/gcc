/* { dg-do assemble } */
/* { dg-options "-fno-plt -masm=intel" } */
/* { dg-require-effective-target masm_intel } */

extern void bar (int);

void
foo (void)
{
  bar (123);
}
