/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-cp -fdump-tree-optimized" } */
/* { dg-skip-if "No alignment restrictions" { { ! natural_alignment_32 } && { ! natural_alignment_64 } } } */

#include <stdint.h>

extern int fail_the_test(void *);
extern int pass_the_test(void *);
extern int diversion (void *);

static int __attribute__((noinline))
foo (void *p)
{
  uintptr_t a = (uintptr_t) p;

  if (a % 4)
    return fail_the_test (p);
  else
    return pass_the_test (p);
}

int
bar (void)
{
  double buf[8] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
  return foo (&buf);
}


/* { dg-final { scan-ipa-dump "Adjusting alignment of param" "cp" } } */
/* { dg-final { scan-tree-dump-not "fail_the_test" "optimized" } } */
