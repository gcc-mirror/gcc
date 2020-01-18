// { dg-do run }

// template parm in a class

#include "../coro.h"

// boiler-plate for tests of codegen
#define USE_AWAIT_TRANSFORM
#include "../coro1-ret-int-yield-int.h"

template <typename T>
class foo
{
  public:
  void use_lambda ()
    {
      int a_copy = 20;
      int a_ref = 10;

      auto f = [&, a_copy]() -> coro1
      {
	co_yield a_ref + a_copy;
	co_return a_ref + a_copy;
      };

      coro1 A = f ();
      A.handle.resume(); // Initial suspend.
      PRINT ("main: [a_copy = 20, a_ref = 10]");
  
      int y = A.handle.promise().get_value();
      if (y != 30)
	{
	  PRINTF ("main: co-yield = %d, should be 30\n", y);
	  abort ();
	}

      a_copy = 5;
      a_ref = 7;

      A.handle.resume(); // from the yield.
      PRINT ("main: [a_copy = 5, a_ref = 7]");

      y = A.handle.promise().get_value();
      if (y != 27)
	{
	  PRINTF ("main: co-ret = %d, should be 27\n", y);
	  abort ();
	}
      PRINT ("use_lambda: about to return");
    }
  ~foo () { PRINT ("foo: DTOR"); }
};

int main ()
{
  foo<int> inst;
  inst.use_lambda();
  return 0;
}
