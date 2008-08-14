// PR c++/34600

namespace N
{
  void foo()
  {
    extern int i = 0; // { dg-error "error: 'i' has both 'extern' and initializer" }
  }
}
