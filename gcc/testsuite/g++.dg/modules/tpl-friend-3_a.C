// { dg-additional-options "-fmodules-ts" }
// declarations followed by friend definition injection

export module foo;
// { dg-module-cmi foo }

void foo (int, void *);
void foo (float, void *);

template <typename T> class TPL
{
  friend void foo (T x, void *p)
  {
    auto *obj = reinterpret_cast<TPL<T> *> (p);

    obj->member = x;
  }

  T member;
};

template class TPL<float>;  // instantiate
