// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }
template <class T>
concept bool True = true;

template <class T>
struct S {
  friend bool operator==(S, int) requires True<T> { return true; }
  friend bool operator==(S, int) requires !True<T> { return true; }
};

int main() {
  S<int> s;
}
