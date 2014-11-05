/* { dg-options "nonpic-addr.o pic-addr.o nonpic-receive-fn-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_addr ();
  pic_addr ();

  if (hit_nonpic_addr != 1)
    abort ();

  if (hit_pic_addr != 1)
    abort ();

  exit (0);
} 
