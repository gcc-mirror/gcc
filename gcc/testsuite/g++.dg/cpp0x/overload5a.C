// PR c++/107461
// { dg-do compile { target c++11 } }

template<class T> T f();
template<class T> decltype(T() + f<int*>()) g(); // #1
template<class T> decltype(T() + f<char>()) g(); // #2, distinct from #1

int main() {
  g<int>(); // { dg-error "ambiguous" }
}
