/* { dg-options "-mips16" } */

void bar (void) {}

void __attribute__((optimize("schedule-insns")))
foo (void)
{
}
