// PR c++/101764
// { dg-do compile { target c++17 } }

void g(...);

template<class>
auto f() {
  return [](auto... ts) {
    g([] { if constexpr (sizeof(ts)); }...);
#if __cpp_concepts
    g(requires { decltype(ts){0}; }...);
#endif
  };
}

int main() {
  f<int>()('a', true);
}
