// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<class T> concept C1 = false;
template<C1 T> concept C2 =  true;  // { dg-error "cannot be constrained" }

void f(C2 auto x) {
}

struct A {} a;

int main() {
  f(a);
}
