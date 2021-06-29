// { dg-additional-options "-fmodules-ts -fcontracts" }

export module foo;
// { dg-module-cmi foo }

void foo (int, void *);
void foo (float, void *);

template <typename T> class TPL
{
  friend void foo (T, void *); // { dg-warning "non-template function" }

  T member;
};

template class TPL<float>;  // instantiate

