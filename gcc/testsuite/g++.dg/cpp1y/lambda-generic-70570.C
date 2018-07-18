// PR c++/70570
// { dg-do assemble { target c++14 } }

template<typename T> void foo(T f) {
  f(1);
}

int main() {
  static const int x = 42;
  foo([](auto y){
    x;
    [](){
      x;
    };
  });
}
