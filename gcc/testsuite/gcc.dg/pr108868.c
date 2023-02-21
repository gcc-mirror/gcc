/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int x;
int vfork (void) __attribute__((__leaf__, __returns_twice__));
int fork (void);
void bar (int, int, int *);

void
foo (void)
{
  int b = 0;
  int r = x ? vfork () : fork ();
  bar (r, x, &b);
}

/* { dg-final { scan-tree-dump "ABNORMAL_DISPATCHER" "optimized" } } */
