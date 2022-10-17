// PR c++/79501
// { dg-do compile { target c++17 } }
// A variant of class-deduction78.C where List and its deduction guides are
// defined at class scope.

using size_t = decltype(sizeof(42));

struct A {
  template<typename T, size_t N = 0>
  struct List {
    T head;
    List<T, N-1> tail;
  };

  template<typename T>
  struct List<T, 0> {};

  template<typename T> List(T) -> List<T, 1>;
  template<typename T, size_t N> List(T, List<T, N>) -> List<T, N+1>;
};

int main() {
  using type = decltype(A::List{0, A::List{1, A::List{2}}});
  using type = A::List<int, 3>;
}
