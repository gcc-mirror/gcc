// N5008 [expr.prim.id.unqual]/p7
// Otherwise, if the unqualified-id appears in the predicate of a contract assertion C (6.10) and the entity is
// (7.1) — a variable declared outside of C of object type T,
// (7.2) — a variable or template parameter declared outside of C of type “reference to T”, or
// (7.3) — a structured binding of type T whose corresponding variable is declared outside of C,
// then the type of the expression is const T
// This tests modifications to the constified things
// { dg-do run { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -O2 -g" }
// { dg-xfail-run-if "PRXXXXXX" { *-*-* } { "-fcontract-checks-outlined" } "" }

#include <cassert>

struct S{
  S(){};
  S(const S&){}
  ~S(){};
  int x = 0;
};

int i = 0;


void f1() pre(const_cast<int&>(i)++) {};
int f2(int n,const S m) pre(const_cast<int&>(n)++)
			pre((const_cast<S&>(m).x = 5))
			post(r: const_cast<int&>(r)++)
			post(r: const_cast<int&>(r)++)
{
  assert (n == 3);
  assert (m.x == 5);

  return 1;
};


S f3(S s) post(r: (const_cast<S&>(r).x = 10) )
{
  return s;
}

int main()
{
  i = 3;
  f1();
  assert (i == 4);

  int j = 2;
  S s;
  int k = f2(j,s);
  assert (k == 3);
  assert (s.x == 0);

  s = f3(s);
  assert (s.x == 10);

}
