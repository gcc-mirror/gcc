// Check that __cxa_vec_[c]ctor returns a pointer to the array
// { dg-do run { target arm*-*-* xscale*-*-* } }

#include <cxxabi.h>

#ifdef ___ARM_EABI__
static void cctor (void * a, void * b)
{
  *(char *) a = *(char *) b
}

int main()
{
  char data;
  char data2;
  char *p;

  p = __cxa_vec_ctor (&data, 1, 1, NULL, NULL);
  if (p != &data)
    return 1;
  p = __cxa_vec_cctor (&data2, &data, 1, 1, cctor, NULL);
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
