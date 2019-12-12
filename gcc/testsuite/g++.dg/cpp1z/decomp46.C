// PR c++/86836
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A {
  int operator*();
  void operator++();
  bool operator!=(A);
};
template <typename> class map {
public:
  A begin();
  A end();
};

template <typename T> void mergemap(map<T> orig, map<T> toadd) {
  for (auto p : toadd)
    auto [orig] = orig;		// { dg-error "use of 'orig' before deduction of 'auto'" }
}				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }

int
main() {
  map<double> x, y;
  mergemap(x, y);
}
