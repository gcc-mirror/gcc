// PR c++/115865
// { dg-do compile }
// { dg-options "-fsanitize=address" }

typedef decltype(sizeof(char)) size_t;

namespace std {
template <class> class initializer_list {
  int *_M_array;
  size_t _M_len;
};
}

int main() {
  std::initializer_list x = { 1, 2, 3 };
}
