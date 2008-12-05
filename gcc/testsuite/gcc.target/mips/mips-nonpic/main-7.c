/* { dg-options "pic-call.o nonpic-addr.o nonpic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  pic_call ();
  nonpic_addr ();

  if (hit_pic_call != 1)
    abort ();

  if (hit_nonpic_addr != 2)
    abort ();

  exit (0);
} 
