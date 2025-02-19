/* { dg-do compile } */
/* { dg-additional-options "-fsanitize=undefined" } */

#include <stdlib.h>

void test ()
{
  int*** ptr = (int ***)malloc(sizeof(int**));
  *ptr = (int **)malloc(sizeof(int*)); /* { dg-warning "dereference of possibly-NULL" } */
  **ptr = (int *)malloc(sizeof(int)); /* { dg-warning "dereference of possibly-NULL" } */

  free(**ptr);
  free(*ptr);
  free(ptr);
}
