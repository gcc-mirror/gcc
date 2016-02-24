// PR c++/69323

template<int VALUE>
struct Outer
{
  struct StupidValueTrick
  {
    template<int VAL> friend struct Outer<VAL>::StupidValueTrick;
  };
};
typedef Outer<42>::StupidValueTrick GoodValue;
GoodValue good;
