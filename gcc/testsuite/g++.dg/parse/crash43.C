// PR c++/34600

namespace N
{
  void foo()
  {
    extern int i = 0; // { dg-error "'i' has both 'extern' and initializer" }
  }
}
