// PR c++/53868
// { dg-do run { target c++11 } }

#if __cplusplus > 201100L
#define THROWING noexcept(false)
#else
#define THROWING
#endif

extern "C" int printf(const char *, ...);
extern "C" void abort();

struct SubobjectInA {
   SubobjectInA();
   ~SubobjectInA();
};

int a;
struct A : SubobjectInA {
   A() = delete;
   A(const A &) = delete;
  A(A &&) = delete;
   A(int);
   ~A();
};

#ifdef DEBUG
#define TRACE_FUNC( ... ) \
{   printf("%s\n", __PRETTY_FUNCTION__); __VA_ARGS__   }
#else
#define TRACE_FUNC( ... ) \
{   __VA_ARGS__   }
#endif

struct Q {
   Q() : q(0)  TRACE_FUNC()
   ~Q() THROWING;
   int q;
};

int main() {
   try { const A &a = Q().q; }
   catch (...) { if (!a) return 0; }
   abort();
}

SubobjectInA::SubobjectInA()  TRACE_FUNC()
SubobjectInA::~SubobjectInA()  TRACE_FUNC()
A::A(int)  TRACE_FUNC(++a;)
A::~A()  TRACE_FUNC(--a;)
Q::~Q() THROWING TRACE_FUNC( throw 0; )
