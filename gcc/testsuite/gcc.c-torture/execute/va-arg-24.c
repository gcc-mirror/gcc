/* Varargs and vectors!  */

#include <stdarg.h>

#define vector __attribute__((vector_size(16)))

const vector unsigned int v1 = {10,11,12,13};
const vector unsigned int v2 = {20,21,22,23};

void foo(int a, ...)
{
  va_list args;
  vector unsigned int v;

  va_start (args, a);
  v = va_arg (args, vector unsigned int);
  if (a != 1 || memcmp (&v, &v1, sizeof (v)) != 0)
    abort ();
  a = va_arg (args, int);
  if (a != 2)
    abort ();
  v = va_arg (args, vector unsigned int);
  if (memcmp (&v, &v2, sizeof (v) != 0))
    abort ();
  va_end (args);
}

int main(void)
{
  foo (1, (vector unsigned int){10,11,12,13}, 2,
       (vector unsigned int){14,15,16,17});
  return 0;
}

