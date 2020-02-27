/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

struct pf {
  unsigned int iu : 2;
};

enum {
  qr, jv, vm, mz,
};

int uh;

void
w9 (struct pf *x2)
{
  /* We ought to know the following based on the bitfield width.  */
  __analyzer_eval (x2->iu >= 0 ); /* { dg-warning "TRUE" } */
  __analyzer_eval (x2->iu < 4 ); /* { dg-warning "TRUE" } */

  switch (x2->iu)
    {
    case qr:
    case jv:
    case vm:
      uh = 0;
      break;

    case mz:
      break;

    default:
      /* We ought to know from the enum values that this code is unreachable,
	 and thus not print anything.
	 TODO(xfail): currently this doesn't work.  */
      __analyzer_eval (x2->iu); /* { dg-bogus "" "" { xfail *-*-* } } */
      __builtin_abort ();
    }
}
