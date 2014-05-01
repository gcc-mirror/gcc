/* { dg-options "-std=c99 -w" } */

#include <stdbool.h>

#define NUM_ELEMS 10

void test_scalar(bool *my_c_bool_ptr);
void test_array(bool *my_bool_array, int num_elems);

int main(int argc, char **argv)
{
  bool my_bool = true;
  bool my_bool_array[NUM_ELEMS];
  int i;

  test_scalar(&my_bool);

  for(i = 0; i < NUM_ELEMS; i+=2)
    my_bool_array[i] = true;
  for(i = 1; i < NUM_ELEMS; i+=2)
    my_bool_array[i] = false;

  test_array(my_bool_array, NUM_ELEMS);
  
  return 0;
}
