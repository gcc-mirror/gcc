// PR c++/65340
// { dg-do compile { target c++14 } }

template <typename Type> Type constant;
namespace reflect {
namespace functors {
  struct recurse { auto operator()(, ); }; // { dg-error "expected" }
}
  auto &recurse = constant < functors :: recurse > ;
}
namespace functors {
struct traverse {
  template <typename Algo, typename Value>
  auto operator()(Algo, Value) -> decltype(reflect
                                           :: recurse(0, 0)); // { dg-error "use of" }
};
}
auto &traverse = constant < functors :: traverse > ;
operator()()->decltype(traverse(0, 0)) // { dg-error "no match|expected" }
