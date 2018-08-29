/* { dg-do compile } */
/* { dg-require-effective-target vect_long_long } */

int N;
long long fn1(void) {
  short i;
  long long a;
  i = a = 0;
  while (i < N)
    a -= i++;
  return a;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
