/* { dg-options "pic-addr.o pic-call.o nonpic-addr.o pic-receive-fn-addr.o nonpic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_nothing ();
  pic_call ();

  if (hit_nonpic_nothing != 2)
    abort ();

  if (hit_pic_call != 1)
    abort ();

  exit (0);
} 
