/* Test the attribute counted_by for pointer fields and its usage in
   bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

#include <stdlib.h>

struct pointer_array {
  int b;
  int *c;
} *p_array;

struct annotated {
  int b;
  int *c __attribute__ ((counted_by (b)));
} *p_array_annotated;

void __attribute__((__noinline__)) setup (int normal_count, int annotated_count)
{
  p_array
    = (struct pointer_array *) malloc (sizeof (struct pointer_array));
  p_array->c = (int *) malloc (normal_count * sizeof (int));
  p_array->b = normal_count;

  p_array_annotated
    = (struct annotated *) malloc (sizeof (struct annotated));
  p_array_annotated->c = (int *) malloc (annotated_count * sizeof (int));
  p_array_annotated->b = annotated_count;

  return;
}

void __attribute__((__noinline__)) test (int normal_index, int annotated_index)
{
  p_array->c[normal_index] = 1;
  p_array_annotated->c[annotated_index] = 2;
}

int main(int argc, char *argv[])
{
  setup (10, 10);   
  test (10, 10);
  return 0;
}

/* { dg-output "36:23: runtime error: index 10 out of bounds for type" } */
