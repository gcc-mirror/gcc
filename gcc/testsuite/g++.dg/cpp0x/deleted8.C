// PR c++/56633
// { dg-do compile { target c++11 } }

struct A {
  A(int) { }
  A(const A&) = delete;
};

struct Test1 {
    A value2{0}; // no problem here
};

template <typename T> // T is not used
struct Test2 {
    A value2{0}; // fails to compile
};

int main() {
    Test1 test;
    Test2<int> test2;
    return 0;
}
