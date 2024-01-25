/* Test for @encode in templates.  */
/* { dg-do run } */
#include <string.h>           
#include <stdlib.h>

template<typename T>
const char *my_encode(int variant)
{
  const char *result;

  switch (variant)
    {
    case 0:
      result = @encode(T);
      break;
    case 1:
      result = @encode(T*);
      break;
    case 2:
      result = @encode(const T*);
      break;
    default:
      result = @encode(int);
      break;
    }

  return result;
}

int main()
{
  if (strcmp (@encode(char), my_encode<char>(0)))
    abort ();

  if (strcmp (@encode(char *), my_encode<char>(1)))
    abort ();

  if (strcmp (@encode(const char *), my_encode<char>(2)))
    abort ();

  if (strcmp (@encode(int), my_encode<char>(3)))
    abort ();

  return 0;
}
