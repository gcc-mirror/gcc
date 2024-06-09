// PR c++/113834
// { dg-do compile { target c++17 } }

template <typename... _Elements> class tuple{};
template <unsigned long __i, typename... _Elements>
__type_pack_element<__i, _Elements...> &get(tuple<_Elements...> &__t) noexcept; // { dg-error "index is out of range" }
tuple<int,int> data;
template <unsigned long Level>
unsigned take_impl(unsigned idx) {
  if constexpr (Level != -1){
    return take_impl<Level - 1>(get<Level - 1>(data)); // { dg-error "" }
  }
  return 0;
}
int main() {
  take_impl<2>(0);
}
