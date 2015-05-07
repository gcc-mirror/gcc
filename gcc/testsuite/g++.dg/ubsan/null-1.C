// { dg-do run }
// { dg-options "-fsanitize=null -Wall -Wno-unused-variable -std=c++11" }

typedef const long int L;

int
main (void)
{
  int *p = 0;
  L *l = 0;

  int &r = *p;
  auto &r2 = *p;
  L &lr = *l;

  // Try an rvalue reference.
  auto &&r3 = *p;

  // Don't evaluate the reference initializer twice.
  int i = 1;
  int *q = &i;
  int &qr = ++*q;
  if (i != 2)
    __builtin_abort ();
}

// { dg-output "reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'const L'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'" }
