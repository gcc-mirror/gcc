// Make sure that A's destructor doesn't affect constexpr
// or exception-spec on D's default constructor.
// { dg-do compile { target c++11 } }

struct A {
  constexpr A() noexcept: i(0) { }
  int i;
  ~A() noexcept(false);
};

struct B: A { };

// Should get static initialization, so no constructor call.
// { dg-final { scan-assembler-not "_ZN1BC1Ev" } }
B b;

struct C { C() noexcept; ~C() noexcept(false); };
struct D: C { };
extern D d;

void *operator new(__SIZE_TYPE__, void*) noexcept;

#define SA(X) static_assert((X),#X)
SA(noexcept(new (&d) D));

struct E: virtual C { };
extern E e;
SA(noexcept (new (&e) E));

struct F { C c; };
extern F f;
SA(noexcept (new (&f) F));
