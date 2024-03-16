// PR c++/101043
// { dg-do compile { target c++20 } }

template<class T>
void f(T t) {
  [&](auto) noexcept(requires { t.g(); }) { }(0);
}

int main() {
  f(0);
}
