/* { dg-do compile } */
#define N 101

typedef unsigned int __uint32_t;

int main(int argc, char **argv)
{
  __uint32_t array[N][N][N];

  const unsigned int next = argc == 3 ? 0 : 1;

  for (unsigned i = next; i < N;  i++)
    array[3][3][i] = array[3][3][i] - 10;

  return array[3][3][argc];
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" } } */
/* { dg-require-effective-target vect_int } */
