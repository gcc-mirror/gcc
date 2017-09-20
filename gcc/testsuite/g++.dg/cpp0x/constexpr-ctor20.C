// PR c++/64954
// { dg-do compile { target c++11 } }

struct A {};
int main() {
    A a;
    constexpr A b = a;
}
