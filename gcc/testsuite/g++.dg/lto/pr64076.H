struct Base {
  virtual void f() = 0;
};

struct X : public Base { };
struct Y : public Base { };
struct Z : public Base { };
struct T : public Base { };

struct S : public X, public Y, public Z
#ifdef XXX
, public T
#endif
{
  void f()
#ifdef XXX
  { }
#endif
  ;
};
