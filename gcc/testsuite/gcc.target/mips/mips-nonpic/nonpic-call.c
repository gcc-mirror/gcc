#include "mips-nonpic.h"
int hit_nonpic_call = 0;
void
nonpic_call ()
{
 pic_nothing ();
 pic_addr ();
 hit_nonpic_call++;
}
