/* PR tree-optimization/105679 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -fno-strict-overflow -fsanitize=shift -fsanitize-coverage=trace-pc -fdump-tree-threadfull1-details" } */

/* Reduced with cvise from attachment 53010: the Linux kernel hit spurious
   -Warray-bounds warnings with -fsanitize=shift.  The sanitizer's
   shift-out-of-bounds check for 1 << irq gives the irq > 31 branch a precise
   never-executed count; the backward threader nevertheless used that block as
   a thread entry, isolating a never-executed path on which irq > 31 indexes
   the 2-element array and drawing the bogus warning.  The bare shift statement
   only exists to feed the sanitizer.

   r13-1891 (the PR105679 fix) rejects paths whose entry edge is probably never
   executed.  This test FAILs before that commit and PASSes with it.  */

struct many_objects
{
  int array[2];
} instance;

int entry;

int
work (unsigned int irq)
{
  1 << irq;
  entry = instance.array[irq];	/* { dg-bogus "above array bounds" } */
  if (irq)
    return 0;
  return 1;
}

/* { dg-final { scan-tree-dump "path entry is probably never executed" "threadfull1" } } */
