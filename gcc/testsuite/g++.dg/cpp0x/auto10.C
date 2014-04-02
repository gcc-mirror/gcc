// Positive test for auto
// { dg-do run { target c++11 } }

#include <typeinfo>
extern "C" void abort();

int main()
{
  if (auto i = 42L)
    {
      if (typeid (i) != typeid (long int))
	abort ();
    }

  while (auto i = 1)
    {
      if (typeid (i) != typeid (int))
	abort ();
      break;
    }
}
