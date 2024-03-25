/* Verify that we don't ICE when generating an out-of-bounds diagram
   when the size of an array is unknown.  */

/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdint.h>

struct sched_class
{
  int64_t f;
};
extern struct sched_class __end_sched_classes[];

int
test ()
{
  const struct sched_class* class = ((__end_sched_classes - 1));
  return class->f; /* { dg-warning "buffer under-read" } */
}

/* We don't care about the content of the diagram, just that we don't
   ICE creating it.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
