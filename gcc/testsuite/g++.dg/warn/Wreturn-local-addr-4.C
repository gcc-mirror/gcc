// PR c++/82600
// { dg-do compile }

void *b[10];

template <int N>
void **
foo (int x)
{
  void **a = b;		// { dg-bogus "address of local variable 'a' returned" }
  return &a[x];
}

void **
bar (int x)
{
  return foo <0> (x);
}
