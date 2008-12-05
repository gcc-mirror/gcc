#include "mips-nonpic.h"
int hit_pic_call = 0;
void
pic_call ()
{
 nonpic_nothing ();
 nonpic_addr ();
 hit_pic_call++;
}
