#include <stdlib.h>

struct union_wrapper
{
  union
  {
    int i;
    void *ptr;
  } u;
};

struct union_wrapper
test_1 (void)
{
  struct union_wrapper uw1;
  uw1.u.ptr = malloc (1024);
  return uw1;
}

struct union_wrapper
test_2 (void)
{
  struct union_wrapper uw2;
  uw2.u.ptr = malloc (1024);
} /* { dg-warning "leak of 'uw2.u.ptr'" } */
