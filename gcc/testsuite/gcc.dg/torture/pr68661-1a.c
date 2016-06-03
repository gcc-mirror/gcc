/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mgeneral-regs-only" } */
/* { dg-additional-sources pr68661-1b.c } */

extern void bar0 (int, int, int, int, int, int, int, int, int)
   __attribute__ ((no_caller_saved_registers));

void
foo (void)
{
  bar0 (0, 1, 2, 3, 4, 5, 6, 7, 8);
}

void
bad (void)
{
  __builtin_abort ();
}
