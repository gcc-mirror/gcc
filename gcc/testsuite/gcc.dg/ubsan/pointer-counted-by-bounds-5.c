/* Test the attribute counted_by for pointer fields and its usage in
   bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */

#include <stdlib.h>

struct annotated {
  int b;
  int *c __attribute__ ((counted_by (b)));
} *p_array_annotated;

void __attribute__((__noinline__)) setup (int annotated_count)
{
  p_array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated));
  p_array_annotated->c = (int *) malloc (annotated_count *  sizeof (int));
  p_array_annotated->b = annotated_count;

  return;
}

void cleanup ()
{
  free (p_array_annotated->c);
  free (p_array_annotated);
}

int main(int argc, char *argv[])
{
  int i;
  setup (10);
  for (i = 0; i < 11; i++)
    p_array_annotated->c[i] = 2; // goes boom at i == 10
  cleanup ();
  return 0;
}


/* { dg-output "34:25: runtime error: index 10 out of bounds for type" } */
