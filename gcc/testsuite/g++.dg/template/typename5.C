// { dg-do compile }

// Origin: ariels@compugen.co.il

// PR c++/2513: typename handling when scope is dependent as
// described in DR108.

template <bool flag> struct Select {
  typedef int Result;
};

template <template<class> class Pred> struct FindType {
  typedef typename Select<true>::Result Result;
};

template <int bits> struct Int {
  template<typename T> struct RightSize {};
  typedef typename FindType<RightSize>::Result type;
};
