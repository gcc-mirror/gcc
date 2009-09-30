// Test using std::function wrapper.
// { dg-do run }
// { dg-options -std=c++0x }

#include <functional>

typedef std::function<int()> FN;

template<typename T>
FN f(T fn)
{
  return [fn]{return fn(2);};
}

int main()
{
  auto fn = f([](int i){return i*21;});

  if (fn() != 42)
    return 1;
  return 0;
}
