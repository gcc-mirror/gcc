/* Test the attribute counted_by for pointer field in anonymous struct/union
   and its usage in __builtin_dynamic_object_size.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"
#ifndef PTR_TYPE
#define PTR_TYPE int
#endif
#ifndef FAM_TYPE
#define FAM_TYPE int
#endif

#define __counted_by(member) \
    __attribute__((__counted_by__(member)))

struct nested_mixed {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  struct {
    PTR_TYPE *pointer __counted_by(n);
    FAM_TYPE c[] __counted_by(b);
  };
} *nested_mixed_annotated;


void __attribute__((__noinline__)) setup (int pointer_array_count,
					  int fam_count)
{
  nested_mixed_annotated
    = (struct nested_mixed *) malloc (sizeof (struct nested_mixed)
				      + fam_count * sizeof (FAM_TYPE));
  nested_mixed_annotated->pointer 
    = (PTR_TYPE *) malloc (sizeof (PTR_TYPE) * pointer_array_count);
  nested_mixed_annotated->b = fam_count;
  nested_mixed_annotated->n = pointer_array_count;
  return;
}

void __attribute__((__noinline__)) test ()
{
  EXPECT(__builtin_dynamic_object_size(nested_mixed_annotated->c, 1),
	 nested_mixed_annotated->b * sizeof (FAM_TYPE));
  EXPECT(__builtin_dynamic_object_size(nested_mixed_annotated->pointer, 1),
	 nested_mixed_annotated->n * sizeof (PTR_TYPE));
}

void cleanup ()
{
  free (nested_mixed_annotated->pointer);
  free (nested_mixed_annotated);
}

int main(int argc, char *argv[])
{
  setup (10,20);   
  test ();
  DONE ();
  cleanup ();
  return 0;
}
