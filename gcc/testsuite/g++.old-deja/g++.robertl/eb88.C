// { dg-do run  }
// { dg-options "-w" }
// Another magic NULL problem.

#include <stddef.h>

int main()
{
  try
    {
      throw(NULL);
    }
  catch (...)
    {
    }
}
