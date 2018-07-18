// { dg-do compile { target c++11 } }

template <class A0, class... As> struct tuple 
{
  tuple<As...> tail;
  template <int Offset, class... More> int apply(const More&... more) {
    return tail.apply<1>(more...); // { dg-error "" } needs .template
  }
};
