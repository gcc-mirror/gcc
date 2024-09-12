// check that contracts can be handled even when exceptions are disabled
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fno-exceptions " }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }
// { dg-output "contract violation in function f at .* a<5" }

#include <exception>
#include <cstdlib>

int terminate_called = 0;
void my_term()
{
    std::exit(0);
}


void f(int a)
  [[ pre : a<5 ]]
  {
  }

int
main ()
{
  std::set_terminate (my_term);
  f(3);
  f(10);
}
