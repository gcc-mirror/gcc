/* { dg-do run } */
/* { dg-additional-options "-fstrict-aliasing" } */

extern void abort();

typedef int A;
typedef float B;

void __attribute__((noipa))
foo(A * p, B *r, long unk, long oh)
{
  for (long i = 0; i < unk; ++i) {
      *p = 1;
      *r = 2;
      if (oh & i)
	break;
      *r = 3;
      *p = 4;
  }
}

int main(void)
{
  union { A x; B f; } u;
  foo(&u.x, &u.f, 1, 1);
  if (u.x != 4) abort();
  foo(&u.x, &u.f, 2, 1);
  if (u.f != 2) abort ();
  return 0;
}
