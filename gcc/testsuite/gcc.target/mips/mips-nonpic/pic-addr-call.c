#include "mips-nonpic.h"
int hit_pic_addr_call = 0;
void
pic_addr_call (void)
{
  hit_pic_addr_call++;
  nonpic_receive_fn_addr (&nonpic_nothing);
}
