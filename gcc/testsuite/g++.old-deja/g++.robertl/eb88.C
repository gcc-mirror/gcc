// Another magic NULL problem.
// Special g++ Options:

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
