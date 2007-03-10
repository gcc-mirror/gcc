// { dg-options "-std=gnu++0x" }
template<class...> struct Tuple { };

template<class... Types> void f(Types&...);
template<class... Types1, class... Types2> void g(Tuple<Types1...>, Tuple<Types2...>);

void h(int x, float& y) 
{
  const int z = x;
  f(x, y, z); // Types is deduced to int, const int, float
  g(Tuple<short, int, long>(), Tuple<float, double>()); // Types1 is deduced to short, int long
                                                        // Types2 is deduced to float, double
}
