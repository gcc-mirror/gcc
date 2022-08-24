// PR c++/53164
// PR c++/105848
// { dg-do link }

template<class T>
void f(T) { }

template<void (*P)(int)>
struct A {
  static void wrap() {
    P(0);
  }
};

template<void (*P)(char)>
void wrap() {
  P(0);
}

template<int>
void g() {
  A<f>::wrap();
  wrap<f>();
}

int main() {
  g<0>();
}
