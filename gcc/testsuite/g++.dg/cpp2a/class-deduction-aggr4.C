// Other testcases from P2082R1
// { dg-do compile { target c++20 } }

template <typename T>
struct X {};
int main() {
  X<int> x1;
  X x2 {x1};
}

template <typename T, int N>
struct A {
  T array[N];
};
A a1 = {{1, 2, 3}}; // should deduce A<int, 3>
A a2 = {"meow"}; // should deduce A<const char, 5>

template <typename T>
struct B {
  T array[2];
};
B b = {0, 1};

template<typename... T>
struct C : T... {};
C c = {
       []{ return 1; },
       []{ return 2; }
};
