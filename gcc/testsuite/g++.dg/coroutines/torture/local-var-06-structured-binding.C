//  { dg-do run }

#include "../coro.h"

struct promise;

struct future
{
  using promise_type = promise;
};

struct promise
{
  template<typename... Args>
  promise (Args&... args) {}
 
  coro::suspend_never initial_suspend() { return {}; }
  coro::suspend_never final_suspend() { return {}; }

  future get_return_object() { return {}; }

  void return_value(int) {}
  void unhandled_exception() {}
};

struct pair
{
  int i;
};

pair 
something ()
{
  return { 1 };
}

future 
my_coro ()
{   
  auto ret = something ();

  if (ret.i != 1)
    abort ();

  auto [ i ] = something ();
  if (i != 1)
    abort ();

  co_return 1;
}

int main ()
{
  my_coro ();
}
