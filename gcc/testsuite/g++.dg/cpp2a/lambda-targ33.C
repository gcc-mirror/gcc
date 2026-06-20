// PR c++/125408
// { dg-do compile { target c++20 } }

struct Role {
    long id;
};

template <typename T> T Obj;
template<int... I> struct A { };
void sink(...);

template <auto Ptr, typename Table>
constexpr int find_member_index() {
  constexpr auto& t = Obj<Table>;
  []<int... Idx>(A<Idx...>) {
    sink([]() {
      using T1 = decltype(&(t.*Ptr));
      return Idx;
    }() ...);
  }(A<1,2>{});
  return 42;
}

auto i = find_member_index<&Role::id, Role>();
