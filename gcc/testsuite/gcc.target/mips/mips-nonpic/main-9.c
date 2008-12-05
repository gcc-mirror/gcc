/* { dg-options "pic-addr.o pic-receive-fn-addr.o nonpic-nothing.o pic-nothing.o" } */

#include "mips-nonpic.h"

main ()
{
  pic_addr ();
  nonpic_nothing ();
  exit (0);
} 
