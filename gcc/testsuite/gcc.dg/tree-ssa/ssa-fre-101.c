/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

int test1 (int i, int j)
{
  int k;
  if (i != j)
    k = i;
  else
    k = j;
  return k;
}

int test2 (int i, int j)
{
  int k;
  if (i != j)
    k = j;
  else
    k = i;
  return k;
}

int test3 (int i, int j)
{
  int k;
  if (i == j)
    k = j;
  else
    k = i;
  return k;
}

int test4 (int i, int j)
{
  int k;
  if (i == j)
    k = i;
  else
    k = j;
  return k;
}

/* We'd expect 4 hits but since we only keep one forwarder the
   VN predication machinery cannot record something for the entry
   block since it doesn't work on edges but on their source.  */
/* { dg-final { scan-tree-dump-times "equal on edge" 2 "fre1" } } */
