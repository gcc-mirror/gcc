/* { dg-do run } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int main (void)
{
  volatile int x1 = 1;
  volatile int x2 = 1;
  int x3 = 2;
  int t = 1;

  t = 3<=(x2|1|x3|x1-1U);

  if (t == 1) {} 
  else { __builtin_abort(); }

  return 0;
}

/* { dg-final { scan-tree-dump-not "abort" "ccp1" } } */
