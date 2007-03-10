// PR c++/20924

template<typename T>
struct x {};

template<typename T, unsigned N>
struct x<T*[N]> {};

int main() {
  x<int> a;
  x<int*[10]> b;
  return 0;
}
