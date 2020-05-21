// PR sanitizer/89869
// { dg-do run { target c++11 } }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct S { S *s = 0; virtual ~S () {} };

void
foo (S *x, S *y)
{
  (x->s ? y : x) = x->s;
}

int
main ()
{
  S a;
  foo (&a, 0);
}
