// { dg-do compile { target c++14 } }
// { dg-additional-options -fcontracts }

struct A {
  const char *p;
  unsigned len;

  constexpr const char&
  operator[] (unsigned i) const
  {
    __contract_assert (i < len);
    return p[i];
  }
};

void f(A a)
{
  /* This shouldn't be an error just because the contract predicate is
     non-constant -- so is the trial evaluation.  */
  const char &c = a[0];
}
