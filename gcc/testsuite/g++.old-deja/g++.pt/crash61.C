// Build don't link:
// Source: Neil Booth, from PR # 106. 4 Dec 2000.

template <bool b> class bar
{
};

class A_a
{
  public:
   static const bool b = true;
};

class B_b
{
  public:
   static const bool b = false;
};

template <class A, class B> class foo
{
};

template <class A, class B>
bar<(A::b || B::b)> do_funky(const foo<A, B>&);

int main()
{
  bar<true> a_bar = do_funky(foo<A_a, B_b>());
}
