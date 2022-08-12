/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1-details" } */

int res;
void foo (int a, int b, int c, int d, int e)
{
  if (a > 100)
    res = 3;
  if (b != 5)
    res = 5;
  if (c == 29)
    res = 7;
  if (d < 2)
    res = 9;
  /* Accounting whoes makes this not catched.  */
#if 0
  if (e != 37)
    res = 11;
#endif
  if (a < 10)
    res = 13;
}

/* { dg-final { scan-tree-dump "SUCCESS" "threadfull1" } } */
