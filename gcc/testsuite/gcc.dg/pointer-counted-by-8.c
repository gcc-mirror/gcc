/* Additional test of the attribute counted_by for pointer field and its usage
   in __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct annotated {
  int b;
  int *c __attribute__ ((counted_by (b)));
};


void __attribute__((__noinline__)) setup (int **ptr, int attr_count)
{
  *ptr = (int *) malloc (sizeof (int) * attr_count);
}

int main(int argc, char *argv[])
{
  struct annotated *f 
    = (struct annotated *) __builtin_malloc (sizeof (struct annotated));
  setup (&f->c, 10);
  f->b = 10;
  EXPECT(__builtin_dynamic_object_size (f->c, 1), 10 * sizeof (int));
  free (f->c);
  free (f);
  DONE ();
}
