/* { dg-do run } */

extern void abort (void);

static inline __attribute__((always_inline)) void
copy(int *restrict a, int *restrict b)
{
  *b = *a;
  *a = 7;
}

void __attribute__((noinline))
floppy(int mat[static 2], unsigned idxs[static 3])
{
  for (int i = 0; i < 3; i++)
    copy(&mat[i%2], &mat[idxs[i]]);
}

int main()
{
  int mat[2] = {10, 20};
  unsigned idxs[3] = {1, 0, 1};
  floppy(mat, idxs);
  if (mat[0] != 7 || mat[1] != 10)
    abort ();
  return 0;
}
