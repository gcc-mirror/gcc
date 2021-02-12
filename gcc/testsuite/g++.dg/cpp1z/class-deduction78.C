// PR c++/98802
// { dg-do compile { target c++17 } }

using size_t = decltype(sizeof(42));

template<typename T, size_t N = 0>
struct List {
    T head;
    List<T, N-1> tail;
};

template<typename T>
struct List<T, 0> {};

template<typename T> List(T) -> List<T, 1>;
template<typename T, size_t N> List(T, List<T, N>) -> List<T, N+1>;

int main() {
  auto list2 = List{0, List{1, List{2}}};
}
