// PR c++/93257
// { dg-do compile { target c++20 } }

template <bool, typename>
consteval void test() {}

int main() {
    test<false, int>();
    return 0;
}
