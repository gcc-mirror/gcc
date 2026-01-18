/* PR c++/122391 */
/* { dg-do "compile" } */

extern "C" {
  template <int> class b; /* { dg-error "with C linkage" } */
  struct {
    typedef b: /* { dg-error "at end of input" } */

/* { dg-excess-errors "" } */
