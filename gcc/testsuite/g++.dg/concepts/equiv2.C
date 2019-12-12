// { dg-do link { target c++17_only } }
// { dg-options "-fconcepts" }


// template<typename T>
// concept bool C() { return true; }


template<typename T>
concept bool C = true;

void f1(C, C);
void f2(C, C);
void f3(C, C);

int main() {
  f1(0, 0);
  f2(0, 0);
  f3(0, 0);
}

void f1(C, C) { }

template<C T1, C T2>
void f2(T1, T2) { }

template<typename T, typename U>
  requires C<T> && C<U>
void f3(T, U) { }
