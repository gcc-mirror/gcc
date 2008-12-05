#include "mips-nonpic.h"
int hit_nonpic_addr = 0;
void
nonpic_addr ()
{
  nonpic_receive_fn_addr (&nonpic_nothing);
  hit_nonpic_addr++;
}
