// PR c++/48873

#include <new>

struct D {
private:
  ~D();
};

template<class T>
T& create();

void f()
{
  D* dp = new (((void*) 0)) D(create<D>()); // #
}
