// PR c++/112437
// { dg-do compile { target c++20 } }

struct S {};
template <class T>
concept Throwable = requires(T x) { throw x; };

bool a = Throwable<S>;
