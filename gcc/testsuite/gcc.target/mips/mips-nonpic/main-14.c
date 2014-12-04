/* { dg-options "nonpic-call.o pic-addr.o pic-receive-fn-addr.o pic-addr-call.o nonpic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_call ();
  pic_addr_call ();

  if (hit_nonpic_call != 1)
    abort ();

  if (hit_pic_addr_call != 1)
    abort ();

  exit (0);
} 
