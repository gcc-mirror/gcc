// PR c++/44808
// { dg-do compile }

struct S
{
  void *a, *b;
  int c;
};

S
foo ()
{
  S x;
  S y = x;
  return x;
}
