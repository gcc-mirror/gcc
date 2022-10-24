/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vectorize" } */

int A[4];
int B[4];

static const char *__attribute__((noipa)) foo()
{
  return "1";
}

int main()
{
  const char *s = foo();

  A[0] = 1000;
  for(int i = 1; i < 4; ++i) {
      B[i] = 0;
      A[i] = 0;
      if(s[0])
	B[i] = 1;
      A[i] = A[i - 1];
  }

  if (A[3] != 1000)
    __builtin_abort ();
  return 0;
}
