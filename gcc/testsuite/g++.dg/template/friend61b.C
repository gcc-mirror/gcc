// PR c++/69323

template<int VALUE>
struct Outer
{
  struct StupidValueTrick
  {
    template<int VAL> friend struct Outer::StupidValueTrick; // { dg-error "not a template" }
  };
};
typedef Outer<42>::StupidValueTrick GoodValue;
GoodValue good;
