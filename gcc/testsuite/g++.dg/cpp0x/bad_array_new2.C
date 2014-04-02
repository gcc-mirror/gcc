// Test for throwing bad_array_new_length on invalid array length
// { dg-do run { target c++11 } }

#include <new>

void * f(int i)
{
  return new int[i]{1,2,3,4};
}

int main()
{
  f(4);				// OK
  try
    {
      f(3);
    }
  catch (std::bad_array_new_length) { return 0; }
  __builtin_abort ();
}
