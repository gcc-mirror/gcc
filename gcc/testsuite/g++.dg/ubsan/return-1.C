// { dg-do run }
// { dg-options "-fsanitize=return -Wno-return-type" }
// { dg-shouldfail "ubsan" }

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
  foo (0);
}

// { dg-output "execution reached the end of a value-returning function without returning a value" }
