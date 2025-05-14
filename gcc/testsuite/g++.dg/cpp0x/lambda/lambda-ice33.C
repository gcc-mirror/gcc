// PR c++/120126
// { dg-do compile { target c++11 } }

template <typename... Args>
int sum(Args... args) {
  return [args...] { // { dg-error "parameter packs not expanded with" }
    typename decltype(args)::type temp;
  };
}
int main() {
  sum(1, 10);
}
