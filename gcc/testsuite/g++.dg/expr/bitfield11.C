// PR c++/37819
// { dg-do compile }

struct A
{
  unsigned int a : 1;
};

bool
foo (A *x, A *y)
{
  x->a = y ? y->a : true;
}
