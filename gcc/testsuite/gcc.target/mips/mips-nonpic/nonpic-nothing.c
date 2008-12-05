#include "mips-nonpic.h"
int hit_nonpic_nothing = 0;
void
nonpic_nothing ()
{
  hit_nonpic_nothing++;
}
