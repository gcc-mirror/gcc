struct Foo
{
  ~Foo ();
};

struct Bar
{
  ~Bar ()
#if __cplusplus < 201103L
  throw(int)
#else
  noexcept(false)
#endif
  ;
  Foo f;
};
