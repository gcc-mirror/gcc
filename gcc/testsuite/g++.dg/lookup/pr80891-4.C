// PR c++/80891 part 4
// Inserting into an immutable overload set

namespace tuples {
template <class, class> void get();
template <int> void get();
}
using tuples::get;
template <class RandomAccessIterator> void make_iterator_vertex_map() {
  RandomAccessIterator a;
  a, get;
}
template <class> void get();
