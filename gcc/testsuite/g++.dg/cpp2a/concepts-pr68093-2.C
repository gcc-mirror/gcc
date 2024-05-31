// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }
template <class T>
concept True = true;

template <class T>
struct S {
  friend bool operator==(S, int) requires (True<T>) { return true; }
  friend bool operator==(S, int) requires (!True<T>) { return true; }
};

int main() {
  S<int> s;
}
