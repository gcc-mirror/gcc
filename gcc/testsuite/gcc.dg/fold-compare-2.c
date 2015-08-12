/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

extern void abort (void);

int a;

int
main(void)
{
  if (a * 1000 < 0)
    abort ();
  if (a * -43 > 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Removing basic block" 2 "fre1" } } */

