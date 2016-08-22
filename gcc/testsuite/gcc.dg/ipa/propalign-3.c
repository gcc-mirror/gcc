/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-cp-alignment -fno-early-inlining -fdump-ipa-cp -fdump-tree-optimized" } */
/* { dg-skip-if "No alignment restrictions" { { ! natural_alignment_32 } && { ! natural_alignment_64 } } } */

#include <stdint.h>

extern int fail_the_test(void *);
extern int pass_the_test(void *);
extern int diversion (void *);

struct somestruct
{
  void *whee;
  void *oops;
};

struct container
{
  struct somestruct first;
  struct somestruct buf[32];
};

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
  struct container c;
  return foo (c.buf);
}


static int
through (struct somestruct *p)
{
  diversion (p);
  return foo (&p[16]);
}

int
bar2 (void)
{
  struct container c;
  through (c.buf);
}

/* { dg-final { scan-ipa-dump-not "Adjusting alignment of param" "cp" } } */
/* { dg-final { scan-tree-dump "fail_the_test" "optimized" } } */
