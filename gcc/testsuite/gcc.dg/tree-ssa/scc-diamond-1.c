/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt-details" } */

/* PR tree-optimization/125557.  The loop selects, from the just-read byte, the
   offset of the next load and advances a pointer -- a data-dependent load
   address recurrence (the loaded value feeds its own next address).  Such a
   loop cannot vectorise, so sink_common_computations_to_bb commons the two
   conditional loads into one reg-offset load and if-converts the diamond into
   branchless selects.  In a loop this fires only at the late sink, after the
   vectorisers have run (gated on fold_before_rtl_expansion_p), so a
   vectorisable loop is vectorised first and left alone.  */

#include <stddef.h>
#include <stdint.h>

const uint8_t *
advance (const uint8_t *ip, size_t tag, const uint8_t *end)
{
  while (ip < end)
    {
      size_t type = tag & 3;
      if (type == 0)
	{
	  size_t nlt = (tag >> 2) + 1;
	  tag = ip[nlt];
	  ip += nlt + 1;
	}
      else
	{
	  tag = ip[type];
	  ip += type + 1;
	}
    }
  return ip;
}

/* The diamond is if-converted (branchless): one selected-offset load remains.  */
/* { dg-final { scan-tree-dump-times "changed to factor out load from" 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 2 "phiopt1" } } */
