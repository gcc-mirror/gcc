/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks -ftree-tail-merge" } */
/* { dg-require-effective-target alloca } */

typedef int aligned __attribute__ ((aligned (64)));
extern void abort (void);

int bar (void *p);

void
foo (void)
{
  char *p = __builtin_alloca (13);
  aligned i;

  if (bar (p) || bar (&i))
    abort ();
}
