// PR c++/111357
// { dg-do compile { target c++11 } }

namespace std {
  template<typename _Tp, _Tp... _Idx>
    struct integer_sequence
    { };

  template<typename _Tp, _Tp _Num>
    using make_integer_sequence
      = integer_sequence<_Tp, __integer_pack(_Num)...>;
}

using std::integer_sequence;
using std::make_integer_sequence;

template<int... V>
void g(integer_sequence<int,V...>)
{}

template<typename ...T>
struct c1
{
  static constexpr int value = 1;
  constexpr operator int() { return value; }
};
template<typename T>
struct R
{
	using S = make_integer_sequence<int,c1<T>{}>;

	R() noexcept(noexcept(g(S())))
	{}
};
int main()
{
        R<int>();
}
