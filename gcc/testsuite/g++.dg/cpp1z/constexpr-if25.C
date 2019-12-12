// PR c++/86740
// { dg-do compile { target c++17 } }

struct Constant
{
  static constexpr int value = 0;
};
template<typename F>
void invokeWithConstant(F &&f)
{
  f(Constant{});
}
int foo()
{
  int count = 0;
  invokeWithConstant
    ([&] (auto id1)
     {
       invokeWithConstant
	 ([&] (auto id2)
	  {
	    if constexpr (id1.value == 0  &&  id2.value == 0)
	      [&] { count = 1; } ();
	  });
     });
  return count;
}
