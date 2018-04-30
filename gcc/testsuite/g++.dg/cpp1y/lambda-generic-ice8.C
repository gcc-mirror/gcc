// PR c++/82230
// { dg-do compile { target c++14 } }

template <class>
  struct c
  {
    template <class>
    void f()
    {
      [](auto) { auto x = [] {}; }(0);
    }
};
int main()
{
  c<int>{}.f<int>();
}
