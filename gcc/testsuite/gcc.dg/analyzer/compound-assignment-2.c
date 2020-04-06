#include <stdlib.h>

struct array_wrapper
{
  void *ptrs[2];
};

struct array_wrapper
test_1 (void)
{
  struct array_wrapper aw1;
  aw1.ptrs[0] = malloc (1024);
  aw1.ptrs[1] = malloc (512);
  return aw1;
}

struct array_wrapper
test_2 (void)
{
  struct array_wrapper aw2;
  aw2.ptrs[0] = malloc (1024);
  aw2.ptrs[1] = malloc (512);
} /* { dg-warning "leak of 'aw2.ptrs.0.'" "leak of element 0" } */
/* { dg-warning "leak of 'aw2.ptrs.1.'" "leak of element 1" { target *-*-* } .-1 } */
