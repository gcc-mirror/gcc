/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

/* Test warning for deprecated typedstream functions.  These functions
   will be removed in the release after 4.6.0, at which point this
   testcase can be removed too.
 */

#include <objc/typedstream.h>

void dummy (void)
{
  TypedStream* t = objc_open_typed_stream_for_file ("dummy", 0); /* { dg-warning "deprecated" } */

  objc_write_object (t, nil); /* { dg-warning "deprecated" } */
  objc_read_object (t, NULL);  /* { dg-warning "deprecated" } */
}
