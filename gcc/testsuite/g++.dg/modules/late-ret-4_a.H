// PR c++/116317
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi {} }

template <typename T> using element_t = int;

template <typename B>
struct reverse_adaptor {
  void pairwise();

  template <typename T>
  auto first(T self) -> element_t<decltype(self)>;

  // We can get the same issue with function parameters
  template <typename T>
  void second(T self, element_t<decltype(self)> second);
};

template <typename B>
void reverse_adaptor<B>::pairwise() {
  reverse_adaptor<int>{};
}
