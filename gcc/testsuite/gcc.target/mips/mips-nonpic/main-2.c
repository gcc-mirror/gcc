/* { dg-options "pic-addr.o nonpic-call.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  nonpic_call ();
  pic_nothing ();

  if (hit_nonpic_call != 1)
    abort ();

  if (hit_pic_nothing != 2)
    abort ();

  exit (0);
} 
