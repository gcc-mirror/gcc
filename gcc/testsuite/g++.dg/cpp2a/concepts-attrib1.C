// PR c++/92739
// { dg-do compile { target concepts } }

template <class T>
concept C = true;

template <class T>
  requires C<T>
[[nodiscard]] int f(T t) {
  return 22;
}

int main() {
  return 0;
}
