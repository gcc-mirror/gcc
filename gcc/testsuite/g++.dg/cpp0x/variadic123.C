// PR c++/52824
// { dg-do compile { target c++11 } }

template<typename G, typename H>
struct foo
{};

template<typename... G>
struct bar : foo<G...>
{};

int main() {
  bar<int, float> f;
}
