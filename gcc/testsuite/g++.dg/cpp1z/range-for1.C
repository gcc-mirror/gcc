// { dg-options "-std=c++1z -pedantic-errors" }

extern "C" int printf (const char *, ...);
#include <initializer_list>

int main()
{
  for (i : {1,2})
    {
      printf ("%d ", i);
    }
}
