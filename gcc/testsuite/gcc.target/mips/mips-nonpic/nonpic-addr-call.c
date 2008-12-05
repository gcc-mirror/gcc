#include "mips-nonpic.h"
int hit_nonpic_addr_call = 0;
void
nonpic_addr_call (void)
{
  hit_nonpic_addr_call++;
  pic_receive_fn_addr (&pic_nothing);
}
