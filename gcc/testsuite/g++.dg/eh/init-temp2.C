// PR c++/34196
// { dg-options "-O -Wuninitialized" }

template <class _Tp> class AutoPtr
{
  _Tp* _M_ptr;

public:
  explicit AutoPtr(_Tp* __p = 0)  : _M_ptr(__p) {}

  ~AutoPtr()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#else
  noexcept(false)
#endif
  { delete _M_ptr; }
};

struct A
{
  A() { }
  ~A()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#else
  noexcept(false)
#endif
  { throw 1; }
};

struct B
{
  virtual ~B()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#else
  noexcept(false)
#endif
  ;
};

B* f (const A &s) { throw 1; }

int
main()
{
  AutoPtr<B> wt(f(A()));
}
