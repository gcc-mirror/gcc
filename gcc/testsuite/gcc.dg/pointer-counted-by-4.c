/* Test the attribute counted_by for pointer field and its usage in
 * __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"
#ifndef PTR_TYPE
#define PTR_TYPE int
#endif
struct pointer_array {
  int b;
  PTR_TYPE *c;
} *p_array;

struct annotated {
  PTR_TYPE *c __attribute__ ((counted_by (b)));
  int b;
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

void __attribute__((__noinline__)) setup (int normal_count, int attr_count)
{
  p_array
    = (struct pointer_array *) malloc (sizeof (struct pointer_array));
  p_array->c = (PTR_TYPE *) malloc (sizeof (PTR_TYPE) * normal_count);
  p_array->b = normal_count;

  p_array_annotated
    = (struct annotated *) malloc (sizeof (struct annotated));
  p_array_annotated->c = (PTR_TYPE *) malloc (sizeof (PTR_TYPE) * attr_count);
  p_array_annotated->b = attr_count;

  p_array_nested_annotated
    = (struct nested_annotated *) malloc (sizeof (struct nested_annotated));
  p_array_nested_annotated->c = (PTR_TYPE *) malloc (sizeof (PTR_TYPE) * attr_count);
  p_array_nested_annotated->b = attr_count;

  return;
}

void __attribute__((__noinline__)) test ()
{
  EXPECT(__builtin_dynamic_object_size(p_array->c, 1), -1);
  EXPECT(__builtin_dynamic_object_size(p_array_annotated->c, 1),
	 p_array_annotated->b * sizeof (PTR_TYPE));
  EXPECT(__builtin_dynamic_object_size(p_array_nested_annotated->c, 1),
	 p_array_nested_annotated->b * sizeof (PTR_TYPE));
}

void cleanup ()
{
  free (p_array->c); 
  free (p_array);
  free (p_array_annotated->c);
  free (p_array_annotated);
  free (p_array_nested_annotated->c);
  free (p_array_nested_annotated);
}

int main(int argc, char *argv[])
{
  setup (10,10);   
  test ();
  DONE ();
  cleanup ();
  return 0;
}
