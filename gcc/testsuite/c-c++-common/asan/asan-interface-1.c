/* Check that interface headers work. */

/* { dg-do run { target { *-*-linux* } } } */

#include <stdbool.h>
#include <sanitizer/asan_interface.h>

int main() {
  char tmp;
  if (__asan_address_is_poisoned((volatile char *)&tmp + 1))
    return 0;
  return 1;
}

