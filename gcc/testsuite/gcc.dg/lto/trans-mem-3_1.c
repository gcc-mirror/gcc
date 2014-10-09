/* { dg-options "-fgnu-tm" } */

#include "trans-mem.h"

extern int i;

int
main()
{
  __transaction_atomic { i = 0; }
}
