// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// template<typename T>
// concept bool C() { return true; }


template<typename T>
concept C = true;

void f1(C auto, C auto);
void f2(C auto, C auto);
void f3(C auto, C auto);

void f1(C auto, C auto) { }

template<C T1, C T2>
void f2(T1, T2) { }

template<typename T, typename U>
  requires C<T> && C<U>
void f3(T, U) { }

int main() {
  f1(0, 0);
  f2(0, 0); // { dg-error "ambiguous" }
  f3(0, 0); // { dg-error "ambiguous" }
}
