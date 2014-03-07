// Test for throwing bad_array_length on invalid array length
// { dg-do run { target c++1y } }

#include <new>

int f(int i)
{
  int ar[i]{1,2,3,4};
  return ar[i-1];
}

void g(int i)
{
  int ar[i];
  ar[0] = 42;
}

int main()
{
  int ok = 0;
  f(4);				// OK
  try { f(3); }			// too small
  catch (std::bad_array_length) { ++ok; }
  try { g(-24); }		// negative
  catch (std::bad_array_length) { ++ok; }

  if (ok != 2)
    __builtin_abort ();
}
