/* { dg-do run { target { le || be } } } */

#include <stdarg.h>
#include <stdlib.h>

long a;

void __attribute__((noipa)) foo (int n, ...)
{
  va_list list;
  va_start (list, n);
  int val = va_arg (list, int);
  if (val != -64)
    abort ();
  va_end (list);
}

int main()
{
  struct __attribute__ ((scalar_storage_order ("little-endian"))) b {
    int : 7;
    int : 2;
    int : 6;
    int : 3;
    int c : 7;
  };
  union __attribute__ ((scalar_storage_order ("little-endian"))) {
    int d;
    struct b bf;
    char e[4];
  } f;
  f.d = a;
  f.e[3] = 7;
  foo (1, f.bf.c);
}
