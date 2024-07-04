/* Test the attribute counted_by and its usage in
 * __builtin_dynamic_object_size: when the counted_by field is negative.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct annotated {
  int b;
  int c[] __attribute__ ((counted_by (b)));
} *array_annotated;

struct nested_annotated {
  struct {
    union {
      int b;
      float f;	
    };
    int n;
  };
  int c[] __attribute__ ((counted_by (b)));
} *array_nested_annotated;

void __attribute__((__noinline__)) setup (int attr_count)
{
  array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated));
  array_annotated->b = attr_count;

  array_nested_annotated
    = (struct nested_annotated *)malloc (sizeof (struct nested_annotated));
  array_nested_annotated->b = attr_count -1;

  return;
}

void __attribute__((__noinline__)) test ()
{
    EXPECT(__builtin_dynamic_object_size(array_annotated->c, 1), 0);
    EXPECT(__builtin_dynamic_object_size(array_nested_annotated->c, 1), 0);
}

int main(int argc, char *argv[])
{
  setup (-10);   
  test ();
  DONE ();
}
