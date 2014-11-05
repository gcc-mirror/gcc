/* { dg-options "pic-addr-call.o nonpic-receive-fn-addr.o nonpic-nothing.o" } */

#include "mips-nonpic.h"

int
main ()
{
  nonpic_nothing ();
  pic_addr_call ();

  if (hit_nonpic_nothing != 1)
    abort ();

  if (hit_pic_addr_call != 1)
    abort ();

  exit (0);
} 
