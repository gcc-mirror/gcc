/* Additional test of the attribute counted_by for pointer field and its usage
   in __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct annotated {
  int b;
  int *c __attribute__ ((counted_by (b)));
};

struct annotated __attribute__((__noinline__)) setup (int attr_count)
{
  struct annotated p_array_annotated;
  p_array_annotated.c = (int *) malloc (sizeof (int) * attr_count);
  p_array_annotated.b = attr_count;

  return p_array_annotated;
}

int main(int argc, char *argv[])
{
  struct annotated x = setup (10);   
  int *p = x.c;
  x = setup (20);
  EXPECT(__builtin_dynamic_object_size (p, 1), 10 * sizeof (int));
  EXPECT(__builtin_dynamic_object_size (x.c, 1), 20 * sizeof (int));
  free (p);
  free (x.c);
  DONE ();
}
