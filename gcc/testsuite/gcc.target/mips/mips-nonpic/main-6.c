/* { dg-options "pic-call.o nonpic-call.o nonpic-addr.o pic-addr.o nonpic-receive-fn-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  pic_call ();
  nonpic_call ();

  if (hit_pic_call != 1)
    abort ();

  if (hit_nonpic_call != 1)
    abort ();

  exit (0);
} 
