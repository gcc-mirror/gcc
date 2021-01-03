// { dg-additional-options "-fmodules-ts" }
// friend injection then definition injection

export module foo;
// { dg-module-cmi foo }

template <typename T> class TPL
{
  friend void foo (T, void *); // { dg-warning "non-template function" }

  T member;
};

template <typename U> class DEF
{
  friend void foo (U x, void *p)
  {
    auto *obj = reinterpret_cast<TPL<U> *> (p);

    obj->member = x;
  }
};

template class TPL<float>;  // instantiate
template class DEF<float>;  // instantiate
