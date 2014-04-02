// { dg-do compile { target c++11 } }
template<class> struct X { static const bool primary = true; };
template<class R, class... ArgTypes> struct X<R(int, ArgTypes...)> { 
  static const bool primary = false;
};
template<class... Types> struct Y { static const bool primary = true; };
template<class T, class... Types> struct Y<T, Types&...> { 
  static const bool primary = false;
};

static_assert (X<int>::primary, "uses primary template");
static_assert (!X<int(int, float, double)>::primary, 
	       "uses partial specialization");
static_assert (X<int(float, int)>::primary, "uses primary template");
static_assert (Y<>::primary, "uses primary template");
static_assert (!Y<int&, float&, double&>::primary, 
	       "uses partial specialization");
static_assert (Y<int, float, double>::primary, "uses primary template");
