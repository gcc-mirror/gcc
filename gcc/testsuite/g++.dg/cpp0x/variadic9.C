// { dg-do compile { target c++11 } }
template<typename T1, typename T2>
struct pair {};

template<typename... Args>
struct tuple {
  static const int value = 0;
};

template<>
struct tuple<pair<int, float> > {
  static const int value = 1;
};

template<typename U>
struct tuple<pair<int, U> > {
  static const int value = 2;
};

template<typename T, typename U>
struct tuple<pair<T, U>, pair<T, U> > {
  static const int value = 3;
};


template<typename... Outer>
struct X {
  template<typename... Inner>
  struct Y
  {
    typedef tuple<pair<Outer, Inner>...> type;
  };
};

int a0[X<int, double>::Y<short, char>::type::value == 0? 1 : -1];
int a1[X<int>::Y<float>::type::value == 1? 1 : -1];
int a2[X<int>::Y<double>::type::value == 2? 1 : -1];
int a3[X<int, int>::Y<double, double>::type::value == 3? 1 : -1];
