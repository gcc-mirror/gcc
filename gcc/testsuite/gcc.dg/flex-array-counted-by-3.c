/* Test the attribute counted_by and its usage in
 * __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct flex {
  int b;
  int c[];
} *array_flex;

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

void __attribute__((__noinline__)) setup (int normal_count, int attr_count)
{
  array_flex
    = (struct flex *)malloc (sizeof (struct flex)
			     + normal_count *  sizeof (int));
  array_flex->b = normal_count;

  array_annotated
    = (struct annotated *)malloc (sizeof (struct annotated)
				  + attr_count *  sizeof (int));
  array_annotated->b = attr_count;

  array_nested_annotated
    = (struct nested_annotated *)malloc (sizeof (struct nested_annotated)
					 + attr_count *  sizeof (int));
  array_nested_annotated->b = attr_count;

  return;
}

void __attribute__((__noinline__)) test ()
{
    EXPECT(__builtin_dynamic_object_size(array_flex->c, 1), -1);
    EXPECT(__builtin_dynamic_object_size(array_annotated->c, 1),
	   array_annotated->b * sizeof (int));
    EXPECT(__builtin_dynamic_object_size(array_nested_annotated->c, 1),
	   array_nested_annotated->b * sizeof (int));
}

int main(int argc, char *argv[])
{
  setup (10,10);   
  test ();
  DONE ();
}
