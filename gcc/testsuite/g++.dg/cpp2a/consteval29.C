// PR c++/103443
// { dg-do compile { target c++20 } }

template<int...>
struct A { };

template<int... Is>
consteval unsigned index_sequence2mask(A<Is...>) {
  if constexpr (sizeof...(Is) == 0u)
    return 0u;
  else
    return ((1u << Is) | ...);
}

template<unsigned Mask = index_sequence2mask(A<1,2,3>{})>
void use_mask();

int main() {
  use_mask();
}
