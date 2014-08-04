// { dg-do run }
// { dg-options "-fsanitize=alignment -Wall -Wno-unused-variable -std=c++11" }

typedef const long int L;
int a = 1;
L b = 2;

int
main (void)
{
  int *p = &a;
  L *l = &b;

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
