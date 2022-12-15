// PR c++/107417
// { dg-do compile { target c++20 } }

template<class... T>
void f() requires (requires (T x) { true; } && ...);

int main() {
  f<int>();
  f<int, void>(); // { dg-error "no match" }
}
