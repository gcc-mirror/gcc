/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized" } */

void free (void *);
void wrapper (void *) __attribute__((transaction_wrap (free)));
void *p;

void foo()
{
  __transaction_relaxed { free (p); }
}

/* We still have one call to free()-- on the uninstrumented path
   everything is as usual.  */
/* { dg-final { scan-tree-dump-times "free" 1 "optimized" } } */

