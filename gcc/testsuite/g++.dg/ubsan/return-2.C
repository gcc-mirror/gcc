// { dg-do run }
// { dg-options "-fsanitize=return -fno-sanitize-recover=return" }

struct S { S (); ~S (); };

S::S () {}
S::~S () {}

int
foo (int x)
{
  S a;
  {
    S b;
    if (x)
      return 1;
  }
}

int
main ()
{
  foo (1);
  foo (14);
}
