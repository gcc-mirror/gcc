// PR c++/58725
// { dg-do compile { target c++11 } }

struct A {
    template<int=0>
    struct B {
        struct C {
            int x = 0;
            double y = x;
        } c;
    };
};
int main() {
    A::B<>();
}
