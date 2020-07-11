// PR c++/85278
// { dg-do compile { target concepts } }

template<typename T>
void f2(T a)
  requires requires (const decltype(a) &&x) { -x; }
{ }

int main() {
  f2<void*>(nullptr); // { dg-error "use of function .*const decltype\\(f2::a\\)&&" }
}
