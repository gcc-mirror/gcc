/* { dg-options "pic-call.o nonpic-addr-call.o nonpic-addr.o nonpic-receive-fn-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  pic_call ();
  nonpic_addr_call ();

  if (hit_pic_call != 1)
    abort ();

  if (hit_nonpic_addr_call != 1)
    abort ();

  exit (0);
} 
