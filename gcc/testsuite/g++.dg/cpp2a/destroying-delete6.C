// PR c++/100588
// { dg-do run { target c++20 } }

extern "C" void abort ();
extern "C" int puts (const char *);
#include <new>

#ifndef DEBUG
#define puts(S)
#endif

class A {
 public:
  A() { throw 42; }
  ~A() { puts("A::~A"); }

  void operator delete(void* p) {
    puts("regular delete invoked");
    ::operator delete(p);
  }

  void operator delete(A* p, std::destroying_delete_t) {
    puts("destroying delete invoked");
    p->~A();
    ::operator delete(p);
    abort ();
  }
};

int main() {
  try {
    new A;
  } catch (int) {
  }
}

