// PR c++/85979
// { dg-do compile { target c++11 } }

template<int N> struct A { };

template<class T>
void f(A<alignof(T)>) { }

#if __cpp_concepts
template<class T>
void g() requires (alignof(T) == 0);
#endif

int main() {
  f<int>(); // { dg-error "no match" }
#if __cpp_concepts
  g<int>(); // { dg-error "no match" "" { target c++20 } }
#endif
}

// { dg-bogus "__alignof__" "" { target *-*-* } 0 }
