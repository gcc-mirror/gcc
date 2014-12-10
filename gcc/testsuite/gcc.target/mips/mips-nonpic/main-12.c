/* { dg-options "nonpic-addr-call.o pic-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_addr_call ();
  pic_addr ();

  if (hit_nonpic_addr_call != 1)
    abort ();

  if (hit_pic_addr != 1)
    abort ();

  exit (0);
} 
