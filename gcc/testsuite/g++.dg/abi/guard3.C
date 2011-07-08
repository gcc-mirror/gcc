// PR c++/45603

extern "C" int __cxa_guard_acquire();

struct A
{
  ~A();
};

A* f()
{
  static A a;
  return &a;
}
