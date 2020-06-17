// PR c++/90732
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-vla" }

/*const*/ int SIZE = 100;

template<typename T>
int foo(T t) {
  char buf[SIZE] = { 24 };
  return [&buf](auto x){ return buf[x]; }(t);
}

int main() {
  if (foo(0) != 24)
    __builtin_abort();
}
