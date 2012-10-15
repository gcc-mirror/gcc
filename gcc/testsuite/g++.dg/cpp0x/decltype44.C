// PR c++/53307
// { dg-do compile { target c++11 } }

template <class...Ts> struct tuple{};

struct funct
{
  template <class T, class...argTs>
  T operator()(T arg1, argTs...)
  {
    return arg1;
  }
};

template <class...>class test;

template < template <class...> class tp,
	   class...arg1Ts,
	   class...arg2Ts> 
class test<tp<arg1Ts...>, tp<arg2Ts...>>
{
 public:
  template <class func>
    auto test_pass(func fun, arg2Ts...arg2s) 
    -> decltype(fun(arg2s...)) 
  {
    return fun(arg2s...);
  }

  template <class func, class...arg3Ts>
    auto testbug(func fun, arg2Ts...arg2s, arg3Ts...arg3s)
    -> decltype(fun(arg2s..., arg3s...)) 
  {
    return fun(arg2s..., arg3s...);
  }
};

int main()
{	
  test<tuple<>, tuple<char, int>> t;
  t.test_pass (funct(), 'a', 2);
  t.testbug (funct(), 'a', 2, "fine");
  t.testbug (funct(), 'a', 2);
}
