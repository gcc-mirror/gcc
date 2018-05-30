// PR c++/85807
// { dg-do compile { target c++11 } }

template <class T>
struct limits
{
  static T max();
};

template< class ScalarT = double >
struct value_statistics_t
{
  double median = limits<double>::max();
};

template< class T > // required
value_statistics_t<> calc()
{
  return {};
}

int main()
{
  value_statistics_t<> wstats = calc<double>();
}
