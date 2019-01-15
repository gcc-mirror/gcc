// { dg-do compile { target c++11 } }

int foo();

__thread int i __attribute__((unused)) = foo();  // { dg-error "14:non-local variable .i. declared .__thread. needs" }

struct S
{
  constexpr S() {}
  ~S();
};

__thread S s __attribute__((unused));  // { dg-error "12:non-local variable .s. declared .__thread." }
