// CWG 2094 - volatile scalars are trivially copyable.
// PR c++/85679
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct S{};

SA(__is_trivially_copyable(S volatile));
SA(__is_trivially_copyable(S volatile[]));
SA(__is_trivially_copyable(S const volatile));
SA(__is_trivially_copyable(S const volatile[]));
SA(__is_trivially_copyable(int volatile));
SA(__is_trivially_copyable(int volatile[]));
SA(__is_trivially_copyable(int const volatile));
SA(__is_trivially_copyable(int const volatile[]));
