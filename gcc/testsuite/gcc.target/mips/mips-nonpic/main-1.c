/* { dg-options "pic-nothing.o nonpic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  nonpic_nothing ();
  pic_nothing ();

  if (hit_nonpic_nothing != 1)
    abort ();

  if (hit_pic_nothing != 1)
    abort ();

  exit (0);
} 
