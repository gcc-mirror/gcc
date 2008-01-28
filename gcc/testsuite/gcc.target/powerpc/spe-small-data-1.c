/* Verify that we don't ICE trying to put SPE data in .sdata2.  */
/* { dg-do run { target { powerpc*-*-linux* && powerpc_spe } } } */
/* { dg-options "-msdata=eabi -mcall-eabi -G 8" } */

#include <spe.h>

__ev64_fs__ x;

int main(void)
{
  x = __ev_fsabs (x);
  return(0);
}

