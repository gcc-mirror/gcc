// PR c++/94808
// { dg-do compile { target concepts } }

template<typename T, typename... Args>
  concept c1 = requires (T t, Args... args) { *t; };
// { dg-message "in requirements with .T t., .Args ... args. .with Args = \{\}; T = int" "" { target *-*-* } .-1 }

static_assert(c1<int>); // { dg-error "failed" }

void f(...);

template<typename... Args>
  concept c2 = requires (Args... args) { f(*args...); };
// { dg-message "in requirements with .Args ... args. .with Args = \{int, char\}" "" { target *-*-* } .-1 }

static_assert(c2<int, char>); // { dg-error "failed" }
