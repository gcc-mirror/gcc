// PR c++/52742
// { dg-do compile { target c++11 } }

void foo() {
  new int[1] {1};
}

template<int A>
void goo() {
  new int[1] {1};
}

int main() {
  foo();
  goo<1>();
}
