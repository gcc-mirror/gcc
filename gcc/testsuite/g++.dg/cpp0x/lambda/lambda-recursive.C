// { dg-do run { target c++11 } }

//#include <iostream>
#include <functional>
#include <cassert>

int main() {

  std::function<int(int)> fib = [&fib] (int n) -> int {
    //std::cerr << "fib(" << n << ")\n";
    if (n <= 2) return 1;
    else        return fib(n-1) + fib(n-2);
  };

  assert(fib(5) == 5);
  assert(fib(10) == 55);

  return 0;
}

