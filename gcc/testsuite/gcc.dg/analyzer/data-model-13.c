#include <stdlib.h>

union
{
  void *ptr_val;
  int int_val;
} global_union;

void test_1 (void)
{
  global_union.ptr_val = malloc (1024);
}

void test_2 (void)
{
  global_union.ptr_val = malloc (1024); /* { dg-message "allocated here" } */
  global_union.int_val = 0; /* { dg-warning "leak of 'global_union.ptr_val' " } */
} 
