struct S { char c; char arr[4]; float f; };

char A[4] = { '1', '2', '3', '4' };

void foo (struct S s)
{
  if (__builtin_memcmp (s.arr, A, 4))
    __builtin_abort ();
}

int main (void)
{
  struct S s;
  __builtin_memcpy (s.arr, A, 4);
  foo (s);
  return 0;
}
