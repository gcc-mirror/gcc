// Another magic NULL problem.
// Special g++ Options: -w

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
