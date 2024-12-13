// PR c++/65790
// { dg-do compile { target c++11 } }

extern "C" int printf(const char*, ...);

namespace std
{
  typedef decltype(sizeof(0)) size_t;

  template<typename _Tp, _Tp... _Idx>
    struct integer_sequence
    {
      typedef _Tp value_type;
      static constexpr size_t size() { return sizeof...(_Idx); }
    };

  template<size_t... _Idx>
    using index_sequence = integer_sequence<size_t, _Idx...>;
}

void g(std::size_t a, std::size_t b, std::size_t c)
{
  printf("%zu, %zu, %zu\n", a, b, c);
}

template <std::size_t... Seq>
void f(std::index_sequence<Seq...>)
{
  g(Seq...);
}

int main()
{
  f(std::index_sequence<0, 1, 2>());
}
