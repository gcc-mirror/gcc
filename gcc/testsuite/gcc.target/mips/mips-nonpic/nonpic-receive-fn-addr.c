#include "mips-nonpic.h"
void
nonpic_receive_fn_addr (void *x)
{
  if (x != &nonpic_nothing)
    abort ();
}
