// PR c++/52830
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fchecking" }
// A version of constexpr-52830.C that uses an intermediate template template
// parameter.

template<bool b> struct eif { typedef void type; };
template<>       struct eif<false> {};

template<class A, class B> struct same
{
  static constexpr bool value = false;
};
template<class A>
struct same<A, A>
{
  static constexpr bool value = true;
};


struct foo {
  template<class T, template<class, class> class SAME = same>
  void func(T && a,
            typename eif<SAME<decltype(a), int&&>::value>::type * = 0);
};

template<class T, template<class, class> class SAME>
void
foo::
func(T && a,
     typename eif<SAME<decltype(a), int&&>::value>::type * )
{
}

void do_stuff()
{
  foo f;
  f.func(12);
}
