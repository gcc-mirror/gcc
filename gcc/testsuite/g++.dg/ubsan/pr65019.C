// PR sanitizer/65019
// { dg-do compile }
// { dg-options "-fsanitize=alignment,object-size,vptr -std=c++11 -O2 -fcompare-debug" }

struct A { };
struct B { };
struct C final {
  C (const A &, int);
  static B *foo (const A &, int = 1);
  virtual ~C ();
  void *c;
};

B *
C::foo (const A &x, int y)
{
  C *d = new C (x, y);
  if (d->c == nullptr)
    delete d;

  return 0;
}

C::~C ()
{
}
