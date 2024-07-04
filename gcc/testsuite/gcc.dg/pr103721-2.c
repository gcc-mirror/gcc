// { dg-do run }
// { dg-require-effective-target thread_fence }
// { dg-options "-O2" }

extern void abort ();
struct S { int x; } a[10];
struct S *b;

int
main ()
{
  int i, j = 0;
  struct S *q = a;

  for (i = 100; --i > 0; )
    {
      struct S *p;
      j++;
      if (j >= 10)
        j = 0;
      p = &a[j];

      if (p == q)
        abort ();
      __atomic_thread_fence (__ATOMIC_SEQ_CST);
      q = p;
    }
  return 0;
}
