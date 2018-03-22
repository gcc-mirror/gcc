// { dg-do compile }
// { dg-options "-fsanitize=return -Wno-return-type" }

struct S { S (); ~S (); };

S::S () {}
S::~S () {}

__attribute__((no_sanitize_undefined))
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

// { dg-final { scan-assembler-not "__ubsan_handle" } }
