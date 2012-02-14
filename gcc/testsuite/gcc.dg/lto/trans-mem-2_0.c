/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target fgnu_tm } */

#include "trans-mem.h"

extern void foobar() __attribute__((transaction_callable));

main()
{
  __transaction_relaxed
    {
      foobar();
    }
}
