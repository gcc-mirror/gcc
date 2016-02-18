// PR c++/68585
// { dg-do compile { target c++11 } }

template<typename T, unsigned N>
  struct array
  {
    T _M_data[N];
  };

template<typename _Tp, _Tp... _Idx>
  struct integer_sequence
  {
  };

struct Pos
{
  unsigned l;
};

template<class T, T... Ints>
constexpr array<Pos, sizeof...(Ints)> make_grid_position(integer_sequence<T, Ints...>)
{
  return {{ Pos{Ints}... }};
}

constexpr array<Pos, 1> make_grid_positions()
{
  return make_grid_position(integer_sequence<unsigned, 0>{});
}

template<class T>
void generate_sudoku(T)
{
  constexpr auto positions = make_grid_positions(); // fail
}

int main()
{
  constexpr auto positions = make_grid_positions(); // ok
  generate_sudoku(1);
}
