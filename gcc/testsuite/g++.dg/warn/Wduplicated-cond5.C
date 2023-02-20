// PR c++/108597
// { dg-do compile }
// { dg-options "-Wduplicated-cond" }

template <typename T>
struct MyStruct {

    void check(int &x) {
        if (&x == &_a) {
        } else if (&x == &_b) {
        }
    }

    int _a;
    int _b;
};
