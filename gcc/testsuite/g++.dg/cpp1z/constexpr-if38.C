// PR c++/94554
// { dg-do compile { target c++17 } }
// { dg-additional-options -Wall }

int meow() { return 1; }
void kitty(int);
template <int (*F)()>
void test() {
    if constexpr (F) {
        kitty(F());
    } else {
        kitty(2);
    }
}
template void test<nullptr>();
template void test<meow>();
