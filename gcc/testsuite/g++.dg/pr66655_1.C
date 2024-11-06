#include "pr66655.h"

extern int32_t g (void);

int32_t S::i;

int32_t
f (void)
{
  int32_t ret = g ();

  S::set (ret);
  return ret;
}
