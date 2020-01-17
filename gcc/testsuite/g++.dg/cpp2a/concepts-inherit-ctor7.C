// PR c++/92552
// { dg-do compile { target concepts } }

template <typename T> struct basic_mixin {
  basic_mixin() requires true;
};
template <typename Cur>
struct mixin : basic_mixin<Cur> {
  using basic_mixin<Cur>::basic_mixin;
};
int main() {
  (void)__is_constructible(mixin<int>);
  // noexcept(mixin<int>()); also triggers ICE
}
