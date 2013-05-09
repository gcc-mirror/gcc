// Test for throwing bad_array_new_length on invalid array length
// { dg-options -std=c++11 }
// { dg-do run }

#include <new>

void * f(int i)
{
  return new int[i];
}

int main()
{
  try
    {
      f(-1);
    }
  catch (std::bad_array_new_length) { return 0; }
  __builtin_abort ();
}
