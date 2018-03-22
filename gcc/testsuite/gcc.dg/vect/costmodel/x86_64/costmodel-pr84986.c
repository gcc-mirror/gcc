/* { dg-do compile } */
/* { dg-require-effective-target vect_long } */

int N;
long fn1(void) {
  short i;
  long a;
  i = a = 0;
  while (i < N)
    a -= i++;
  return a;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
