// PR sanitizer/64632
// { dg-do run }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct S
{
  S () : a(0) {}
  int a;
  int f () { return a; }
  virtual int v () { return 0; }
};

struct X : virtual S
{
  int v () { return 2; }
};

int
main ()
{
  X x;
  return x.f ();
}
