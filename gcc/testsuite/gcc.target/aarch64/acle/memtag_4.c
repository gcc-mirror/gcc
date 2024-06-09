/* { dg-do compile } */
/* { dg-options "-march=armv9-a+memtag  --param ggc-min-expand=0 --param ggc-min-heapsize=0" } */
/* PR target/108174 */
/* Check to make sure that the builtin functions are not GC'ed away. */
#include "arm_acle.h"

void g(void)
{
  const char *c;
  __arm_mte_increment_tag(c , 0 );
}
void h(void)
{
  const char *c;
  __arm_mte_increment_tag( c,0);
}
