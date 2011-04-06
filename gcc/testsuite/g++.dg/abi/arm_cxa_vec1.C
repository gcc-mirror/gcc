// Check that __cxa_vec_[c]ctor returns a pointer to the array
// { dg-do run { target arm*-*-* } }

#include <cxxabi.h>

#ifdef __ARM_EABI__
using namespace __cxxabiv1;
static __cxa_cdtor_return_type cctor (void * a, void * b)
{
  *(char *) a = *(char *) b;
  return a;
}

int main()
{
  char data;
  char data2;
  char *p;

  p = (char *) __cxa_vec_ctor (&data, 1, 1, NULL, NULL);
  if (p != &data)
    return 1;
  p = (char *) __cxa_vec_cctor (&data2, &data, 1, 1, cctor, NULL);
  if (p != &data2)
    return 1;

  return 0;
}
#else
int main()
{
  return 0;
}
#endif
