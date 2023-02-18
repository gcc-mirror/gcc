// PR c++/107461
// { dg-do compile { target c++11 } }

int f(...);
template<class T> decltype(T() + f(0)) g(); // #1

char f(int);
template<class T> decltype(T() + f(0)) g(); // #2, distinct from #1

int main() {
  g<int>(); // { dg-error "ambiguous" }
}
