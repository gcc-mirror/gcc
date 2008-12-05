/* { dg-options "nonpic-call.o pic-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  nonpic_call ();
  pic_addr ();

  if (hit_nonpic_call != 1)
    abort ();

  if (hit_pic_addr != 2)
    abort ();

  exit (0);

} 
