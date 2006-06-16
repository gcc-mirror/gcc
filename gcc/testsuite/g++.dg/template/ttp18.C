template <template <typename> class T>
void f(T<int>) {}

template <typename T>
union U {};

void g() {
  f(U<int>());
}

