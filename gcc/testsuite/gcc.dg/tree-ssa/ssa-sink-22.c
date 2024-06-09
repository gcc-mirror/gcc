/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink1-details" } */

extern void abort (void);

int foo (int x, int y, int f)
{
  int tem = x / y;
  if (f)
    abort ();
  return tem;
}

/* { dg-final { scan-tree-dump-not "Sinking" "sink1" } } */
