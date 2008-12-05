#include "mips-nonpic.h"
void
pic_receive_fn_addr (void *x)
{
  if (x != &pic_nothing)
    abort ();
}
