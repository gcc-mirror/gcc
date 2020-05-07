/* { dg-do run } */

extern void abort();

typedef int A;
typedef float B;

void __attribute__((noinline,noclone))
foo(A *p, B *q, long unk)
{
  for (long i = 0; i < unk; ++i) {
      *p = 1;
      q[i] = 42;
  }
}

int main(void)
{
  union { A x; B f; } u;
  foo(&u.x, &u.f, 1);
  if (u.f != 42) abort();
  return 0;
}
