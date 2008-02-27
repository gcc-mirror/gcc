// PR c++/35368
// { dg-require-visibility "" }

#pragma GCC visibility push (hidden)

struct A
{
  A();
  virtual ~A() { }
};

A::A()
{
}

void foo(A *a)
{
  delete a;
}

// { dg-final { scan-not-hidden "_ZTVN10__cxxabiv117__class_type_infoE" } }
// { dg-final { scan-hidden "_ZTI1A" } }
// { dg-final { scan-hidden "_ZTV1A" } }
