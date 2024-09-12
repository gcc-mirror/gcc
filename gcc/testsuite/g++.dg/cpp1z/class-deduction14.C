// { dg-do compile { target c++17 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

template<class T> struct container {
  container(T t) {}
  template<class Iter> container(Iter beg, Iter end);
};
template<class Iter>
container(Iter b, Iter e)	// { dg-message "iterator_traits.int" }
  -> container<typename std::iterator_traits<Iter>::value_type>;
std::vector<double> v = { /* ... */ };
container c(7);		  // OK, deduces int for T
auto d = container(v.begin(), v.end()); // OK, deduces double for T
container e{5, 6};		// { dg-error "" } int is not an iterator
