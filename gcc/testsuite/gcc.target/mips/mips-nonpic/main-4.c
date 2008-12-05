/* { dg-options "pic-addr.o pic-receive-fn-addr.o nonpic-addr-call.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  nonpic_addr_call ();
  pic_nothing ();

  if (hit_nonpic_addr_call != 1)
    abort ();

  if (hit_pic_nothing != 1)
    abort ();

  exit (0);
} 
