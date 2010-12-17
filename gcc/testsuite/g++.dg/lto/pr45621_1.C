#include "pr45621.h"

void
S::v1 ()
{
  v2 ();
}

void
S::m ()
{
  v1 ();
}
