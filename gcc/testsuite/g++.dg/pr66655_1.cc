#include "pr66655.h"

extern int g (void);

int S::i;

int
f (void)
{
  int ret = g ();

  S::set (ret);
  return ret;
}
