/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void K (int *gpwgts, int *badminpwgt, int *badmaxpwgt)
{
  int i;
  for (i = 0; i < 10; i += 2) {
    badminpwgt[i] = badminpwgt[i+1] = gpwgts[i]+gpwgts[i];
    badmaxpwgt[i] = badmaxpwgt[i+1] = gpwgts[i]+gpwgts[i];
  }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

