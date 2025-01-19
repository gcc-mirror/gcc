// PR c++/118139
// { dg-do compile { target c++20 } }

template<class T>
struct CW {
  using V = typename decltype(T())::type;
  static void operator=(int) requires requires(V x) { x; } {} // { dg-error {requires requires\(typename decltype\(T\(\)\)::type x\)} }
};
