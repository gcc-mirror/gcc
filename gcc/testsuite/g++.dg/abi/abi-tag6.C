// PR c++/60642

struct __attribute((abi_tag("test"))) foo
{
  void f();
  virtual ~foo();
};

template<typename>
struct __attribute((abi_tag("test"))) bar
{
  void f();
  virtual ~bar();
};

int main()
{
  foo f;
  f.f();

  bar<int> b;
  b.f();
}

// { dg-final { scan-assembler "_ZTV3barB4testIiE" } }
