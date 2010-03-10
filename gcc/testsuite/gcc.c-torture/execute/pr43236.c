/* { dg-options "-ftree-loop-distribution" } */
extern void abort(void);
extern void *memset(void *s, int c, __SIZE_TYPE__ n);
extern int memcmp(const void *s1, const void *s2, __SIZE_TYPE__ n);
/*extern int printf(const char *format, ...);*/

int main()
{
  char A[30], B[30], C[30];
  int i;

  /* prepare arrays */
  memset(A, 1, 30);
  memset(B, 1, 30);

  for (i = 20; i-- > 10;) {
    A[i] = 0;
    B[i] = 0;
  }

  /* expected result */
  memset(C, 1, 30);
  memset(C + 10, 0, 10);

  /* show result */
/*  for (i = 0; i < 30; i++)
    printf("%d %d %d\n", A[i], B[i], C[i]); */

  /* compare results */
  if (memcmp(A, C, 30) || memcmp(B, C, 30)) abort();
  return 0;
}
