// { dg-do run }
// { dg-options "-fsanitize=return" }

#include <stdio.h>

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
  fputs ("UBSAN TEST START\n", stderr);

  foo (1);
  foo (14);

  fputs ("UBSAN TEST END\n", stderr);
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
