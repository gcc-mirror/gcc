// PR c++/55564
// { dg-options -std=c++11 }

template <typename T, decltype(sizeof(T)) N>
auto array_size(T(&)[N]) -> decltype(N) { return N; }

int main() {
  int simple[4] = {};
  int result =  array_size(simple);

  return result;
}
