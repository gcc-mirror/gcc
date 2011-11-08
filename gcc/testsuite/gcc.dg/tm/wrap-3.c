/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized" } */

void free (void *);
void wrapper (void *) __attribute__((transaction_wrap (free)));
void *p;

void foo()
{
  __transaction_relaxed { free (p); }
}

/* { dg-final { scan-tree-dump-times "free" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
