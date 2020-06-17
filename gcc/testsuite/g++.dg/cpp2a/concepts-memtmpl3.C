// PR c++/92582
// { dg-do compile { target concepts } }

template <class a, class> concept b = true;
template <typename> struct A {
  template <typename c> static constexpr bool d = b<c, int>;
  template <typename c> static void f(c) requires d<c>;
};
int main()
{
  A<void>::f(0);
}
