/* { dg-options "nonpic-addr.o nonpic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_addr ();
  pic_nothing ();

  if (hit_nonpic_addr != 1)
    abort ();

  if (hit_pic_nothing != 1)
    abort ();

  exit (0);
} 
