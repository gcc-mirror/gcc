// { dg-options "-std=gnu++11" }
template<typename T1, typename T2>
struct pair {};

template<typename... Args>
struct tuple {
  static const int value = 0;
};

template<>
struct tuple<pair<int, float> > { };

template<typename... Outer>
struct X {
  template<typename... Inner>
  struct Y
  {
    typedef tuple<pair<Outer, Inner>...> type; // { dg-error "mismatched argument pack lengths" }
  };
};

X<int, double>::Y<short, char, double>::type honk; 
