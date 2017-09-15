// { dg-do run }
// { dg-options "-std=c++17 -fconcepts" }


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

template<C T>
void f2(T, T) { }

template<typename T>
  requires C<T>
void f3(T, T) { }
