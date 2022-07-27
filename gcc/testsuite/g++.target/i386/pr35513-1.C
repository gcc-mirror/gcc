// { dg-do run { target { *-*-linux* && property_1_needed } } }
// { dg-options "-O2 -mno-direct-extern-access" }

#include <iostream>

class Bug
{
};

int throw_bug()
{
  throw Bug();

  return 0;
}

int main()
{
  try {
      std::cout << throw_bug();
  } catch (Bug bug) {
  };

  return 0;
}
