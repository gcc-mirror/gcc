#include "mips-nonpic.h"
int hit_pic_addr = 0;
void
pic_addr ()
{
  pic_receive_fn_addr (&pic_nothing);
  hit_pic_addr++;
}
