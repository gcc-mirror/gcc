/* { dg-options "nonpic-addr-call.o pic-receive-fn-addr.o pic-addr-call.o nonpic-receive-fn-addr.o pic-nothing.o nonpic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  nonpic_addr_call ();
  pic_addr_call ();

  if (hit_nonpic_addr_call != 1)
    abort ();

  if (hit_pic_addr_call != 1)
    abort ();

  exit (0);
}
