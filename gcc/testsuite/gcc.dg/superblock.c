/* { dg-do compile } */
/* { dg-options "-O2 -fno-asynchronous-unwind-tables -fsched2-use-superblocks -fdump-rtl-sched2 -fdump-rtl-bbro" } */

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

/* { dg-final { scan-rtl-dump-times "0 uses" 0 "bbro"} } */
/* { dg-final { scan-rtl-dump-times "ADVANCING TO" 2 "sched2"} } */
/* { dg-final { cleanup-rtl-dump "bbro" } } */
/* { dg-final { cleanup-rtl-dump "sched2" } } */

