// PR c++/77655
// { dg-do compile { target c++11 } }

template <class F> void g(F);
template <class... A>
auto h(A &&... a) -> decltype(g(0, g<decltype(a)>(a)...)) {  // { dg-error "" }
  h([] {});  // { dg-error "no matching" }
}

int main() { 
  h(); 
  return 0; 
}
