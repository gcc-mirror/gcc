// PR c++/37819
// { dg-do compile }

struct A
{
  unsigned int a : 1;
};

void
foo (A *x, A *y)
{
  x->a = y ? y->a : true;
}
