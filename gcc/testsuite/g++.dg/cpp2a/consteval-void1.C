// PR c++/93257
// { dg-do compile { target c++2a } }

template <bool, typename>
consteval void test() {}

int main() {
    test<false, int>();
    return 0;
}
