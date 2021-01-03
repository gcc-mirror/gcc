// DR 2303
// PR c++/97453
// { dg-do compile { target c++11 } }

template <typename... T> struct A;
template <> struct A<>
{
};
template <typename T, typename... Ts> struct A<T, Ts...> : A<Ts...>
{
};
struct B : A<int, int>
{
};

struct C : A<int, int>, A<int> //  { dg-warning "direct base .A<int>. inaccessible in .C. due to ambiguity" }
{
};

struct D : A<int>, A<int, int> //  { dg-warning "direct base .A<int>. inaccessible in .D. due to ambiguity" }
{
};
template <typename... T>
void
f (const A<T...> &)
{
  static_assert (sizeof...(T) == 2, "it should duduce to A<int,int>");
}


void
g ()
{
  f (B{});
  f (C{});
  f (D{});
}
