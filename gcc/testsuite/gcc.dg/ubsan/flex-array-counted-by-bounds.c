/* Test the attribute counted_by and its usage in
   bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

#include <stdlib.h>

struct flex {
  int b;
  int c[];
} *array_flex;

struct annotated {
  int b;
  int c[] __attribute__ ((counted_by (b)));
} *array_annotated;

void __attribute__((__noinline__)) setup (int normal_count, int annotated_count)
{
  array_flex
    = (struct flex *)malloc (sizeof (struct flex)
			     + normal_count *  sizeof (int));
  array_flex->b = normal_count;

  array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated)
				  + annotated_count *  sizeof (int));
  array_annotated->b = annotated_count;

  return;
}

void __attribute__((__noinline__)) test (int normal_index, int annotated_index)
{
  array_flex->c[normal_index] = 1;
  array_annotated->c[annotated_index] = 2;
}

int main(int argc, char *argv[])
{
  setup (10, 10);   
  test (10, 10);
  return 0;
}

/* { dg-output "36:21: runtime error: index 10 out of bounds for type" } */
