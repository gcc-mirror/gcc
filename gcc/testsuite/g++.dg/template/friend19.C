// { dg-do compile }

// Origin: Benjamin Li <benxbli@yahoo.com>

// PR c++/11030: Template substitution of friend class that is
// a specialization.

template <int S>
struct A
{
  void func(void);
};

template <class T>
class C
{
  static void private_func(void) {}
public:
  friend class A<512>;
};

template <int S>
void A<S>::func(void)
{
  C<void>::private_func();
}

template class A<512>;
