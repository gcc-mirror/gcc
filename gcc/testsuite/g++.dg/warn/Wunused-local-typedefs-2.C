// Origin PR c++/33255
// { dg-options "-Wunused" } <-- should trigger -Wunused-local-typedefs
// { dg-do compile { target c++11 } }

template <typename C>
struct structure
{
    typename C::type val;
};

int
main()
{
  struct context
  {
      using type = int;
  };

  return structure<context>{42}.val;
}
