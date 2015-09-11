// PR c++/34196
// { dg-options "-O -Wuninitialized" }

template <class _Tp> class AutoPtr
{
  _Tp* _M_ptr;

public:
  explicit AutoPtr(_Tp* __p = 0)  : _M_ptr(__p) {}

  ~AutoPtr() throw(int) { delete _M_ptr; }
};

struct A
{
  A() { }
  ~A() throw(int) { throw 1; }
};

struct B
{
  virtual ~B() throw(int);
};

B* f (const A &s) { throw 1; }

int
main()
{
  AutoPtr<B> wt(f(A()));
}
