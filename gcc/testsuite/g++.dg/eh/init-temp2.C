// PR c++/34196
// { dg-options "-O -Wuninitialized" }

template <class _Tp> class AutoPtr
{
  _Tp* _M_ptr;

public:
  explicit AutoPtr(_Tp* __p = 0)  : _M_ptr(__p) {}

  ~AutoPtr() { delete _M_ptr; }
};

struct A
{
  A() { }
  ~A() { throw 1.0; }
};

struct B
{
  virtual ~B();
};

B* f (const A &s) { throw 1; }

int
main()
{
  AutoPtr<B> wt(f(A()));
}
