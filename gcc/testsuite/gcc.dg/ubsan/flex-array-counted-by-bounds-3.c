/* Test the attribute counted_by and its usage in bounds
   sanitizer. when counted_by field is negative value.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

#include <stdlib.h>

struct annotated {
  int b;
  int c[] __attribute__ ((counted_by (b)));
} *array_annotated;

void __attribute__((__noinline__)) setup (int annotated_count)
{
  array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated));
  array_annotated->b = annotated_count;

  return;
}

void __attribute__((__noinline__)) test (int annotated_index)
{
  array_annotated->c[annotated_index] = 2;
}

int main(int argc, char *argv[])
{
  setup (-3);   
  test (2);
  return 0;
}

/* { dg-output "24:21: runtime error: index 2 out of bounds for type" } */
