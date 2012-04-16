// PR c++/17763

template <typename U> struct Outer {
    struct Inner {};
    Inner foo();
};

typedef int X;
typedef Outer<X> XOuter;

int main() {
  Outer<int>  ab;
  ab.foo() == 1; // { dg-error "operand types are 'Outer<int>::Inner' and 'int'" }
}
