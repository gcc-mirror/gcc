#include "mips-nonpic.h"
int hit_pic_nothing = 0;
void
pic_nothing ()
{
  hit_pic_nothing++;
}
