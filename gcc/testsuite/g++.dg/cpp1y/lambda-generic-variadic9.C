// PR c++/84126
// { dg-do compile { target c++14 } }

template <class... Ts>
void sink(Ts...);

template <typename T>
int bar(T&); // ICE with reference, work with just T

template <typename T >
void foo(T){
  [](auto ... k){
    sink (bar(k) ...);
  }(0);
}

int main() {
  foo(0);
}
