// PR c++/66850
// Each namespace contains an otherwise standalone test case, none of which
// should cause an ICE.

namespace X {
  template <template <typename U, U> class> struct Sort;

  template <template <typename U, U> class Comparator>
  struct Sort
  {
    template <int I>
    struct less_than
    {
      Comparator<int, I> a;
    };
  };
}

namespace Y {
  template <typename C, C> struct integral_constant {};

  template <typename T, template <typename U, U> class> struct Sort;

  template <template <typename U, U> class Comparator>
  struct Sort<int, Comparator>
  {
      template <int I> struct less_than:
          integral_constant<bool, Comparator<int, I>::value> {};
  };
}

namespace Z {
  template <typename T, template <typename U, U> class> struct Sort;

  template <template <typename U, U> class Comparator>
  struct Sort<int, Comparator>
  {
    template <int I>
    struct less_than
    {
      Comparator<int, I> a;
    };
  };
}
