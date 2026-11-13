/* { dg-additional-options "-Wno-compare-distinct-pointer-types" { target c } } */

#include "analyzer-decls.h"

const void *
hide_from_optimizer (void *ptr)  __attribute__((noinline));

const void *
hide_from_optimizer (void *ptr)
{
  return ptr;
}

unsigned char global_buf[1024];

void
test_pointer_le (unsigned *p, int i, int j)
{
  unsigned char local_declared_before_local_buf;
  unsigned char local_buf[16];
  unsigned char local_declared_after_local_buf;

  __analyzer_eval (p <= hide_from_optimizer (p)); // { dg-warning "TRUE" }
  __analyzer_eval (local_buf <= &local_buf[0]); // { dg-warning "TRUE" }

  /* Same concrete base region.  */
  {
    // Same concrete offsets
    __analyzer_eval (local_buf <= hide_from_optimizer (&local_buf[0])); // { dg-warning "TRUE" }
    
    // Different concrete offsets
    __analyzer_eval (&local_buf[0] <= hide_from_optimizer (&local_buf[1])); // { dg-warning "TRUE" }
    __analyzer_eval (&local_buf[1] <= hide_from_optimizer (&local_buf[0])); // { dg-warning "FALSE" }

    // Same symbolic offset
    __analyzer_eval (&local_buf[i] <= hide_from_optimizer (&local_buf[i])); // { dg-warning "TRUE" }

    // Possibly different symbolic offset
    {
      __analyzer_eval (&local_buf[i] <= hide_from_optimizer (&local_buf[j])); // { dg-warning "UNKNOWN" }
      if (i == j)
	__analyzer_eval (&local_buf[i] <= hide_from_optimizer (&local_buf[j])); // { dg-warning "TRUE" }
      else
	__analyzer_eval (&local_buf[i] <= hide_from_optimizer (&local_buf[j])); // { dg-warning "UNKNOWN" }
    }
  }

  /* Different concrete base regions.  */
  {
    __analyzer_eval (local_buf <= hide_from_optimizer (global_buf)); // { dg-warning "UNKNOWN" }

    // We can't compare the addresses of locals
    __analyzer_eval (&local_buf[0] <= hide_from_optimizer (&local_declared_before_local_buf)); // { dg-warning "UNKNOWN" }
    __analyzer_eval (&local_buf[15] <= hide_from_optimizer (&local_declared_after_local_buf)); // { dg-warning "UNKNOWN" }
  }

  /* Concrete vs symbolic where we know they're different
     (buf is local, so param p can't point to it).  */
  __analyzer_eval (local_buf <= hide_from_optimizer (p)); // { dg-warning "UNKNOWN" "ideal behavior" { xfail *-*-* } }
  // { dg-message "FALSE" "current behavior" { target *-*-* } .-1 }

  /* Concrete vs symbolic where we don't know they're different
     (glob is global, so param p could point to it).  */
  __analyzer_eval (global_buf <= hide_from_optimizer (p)); // { dg-warning "UNKNOWN" }

  // What about symbolic, could different offsets be the same?
  // TODO: what about overflow of locals, to try to alias (undefined behavior)
}
