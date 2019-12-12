// PR c++/90832 - endless recursion when evaluating sizeof.
// { dg-do compile { target c++11 } }

class B
{
 template <typename T> friend struct A;
 B() {}
};

template <typename T>
struct A
{
 A() noexcept(sizeof(B{})) { }
};

struct C
{
 C()
 {
 static_assert( sizeof(A<int>{}), "" );
 }
};
