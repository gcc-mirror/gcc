/* Test the attribute counted_by for pointer fields and its usage in
   bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-fsanitize=bounds" } */
/* { dg-output "index 10 out of bounds for type 'A \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 11 out of bounds for type 'A \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

#include <stdlib.h>

union A {
  int a;
  float b;
};
#define PTR_TYPE union A 
struct annotated {
  int b;
  PTR_TYPE *c __attribute__ ((counted_by (b)));
} *p_array_annotated;

struct nested_annotated {
  PTR_TYPE *c __attribute__ ((counted_by (b)));
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
} *p_array_nested_annotated;

void __attribute__((__noinline__)) setup (int annotated_count)
{
  p_array_annotated
    = (struct annotated *) malloc (sizeof (struct annotated));
  p_array_annotated->c = (PTR_TYPE *) malloc (annotated_count * sizeof (PTR_TYPE));
  p_array_annotated->b = annotated_count;

  p_array_nested_annotated
    = (struct nested_annotated *) malloc (sizeof (struct nested_annotated));
  p_array_nested_annotated->c = (PTR_TYPE *) malloc (sizeof (PTR_TYPE) * annotated_count);
  p_array_nested_annotated->b = annotated_count;

  return;
}

void cleanup ()
{
  free (p_array_annotated->c);
  free (p_array_annotated);
  free (p_array_nested_annotated->c);
  free (p_array_nested_annotated);
}

int main(int argc, char *argv[])
{
  setup (10);   
  p_array_annotated->c[10].a = 2;
  p_array_nested_annotated->c[11].a = 3;
  cleanup ();
  return 0;
}
