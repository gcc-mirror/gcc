// PR c++/33842
// { dg-do compile }

template<typename T> struct A
{
  A<__builtin_offsetof(T, x)>();	// { dg-error "type/value mismatch|offsetof\\(T, x\\)" }
};

template<typename T> struct B
{
  B<__builtin_offsetof(T, x.y)>();	// { dg-error "type/value mismatch|offsetof\\(T, x.y\\)" }
};

template<typename T> struct C
{
  C<__builtin_offsetof(T, x[6])>();	// { dg-error "type/value mismatch|offsetof\\(T, x\\\[6\\\]\\)" }
};

template<typename T> struct D
{
  D<__builtin_offsetof(T, x.y[6].z)>();	// { dg-error "type/value mismatch|offsetof\\(T, x.y\\\[6\\\].z\\)" }
};

struct E { int x; };

template<typename T> struct F
{
  F<__builtin_offsetof(E, x)>();	// { dg-error "type/value mismatch|offsetof\\(E, x\\)" }
};
