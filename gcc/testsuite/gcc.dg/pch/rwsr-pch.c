#include "rwsr-pch.h"
extern int printf (const char *, ...);
int main (void) {
  long long val = rwsr ();
  printf ("%lld\n", val);
  return 0;
}
