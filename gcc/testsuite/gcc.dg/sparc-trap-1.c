/* PR target/15693 */
/* { dg-do compile { target sparc*-*-* } } */
/* { dg-options "-O2" } */

/* This used to fail on SPARC at -O2 because the combiner
   produces a compare insn that was not rematched by the
   compare expander.  */

static __inline__ __attribute__ ((always_inline))
int page_mapping (unsigned flags)
{
  if (1u & (flags >> 16))
    return 1;
  return 0;
}
void install_page (unsigned flags)
{
  if (__builtin_expect (!page_mapping (flags), 0))
    __builtin_trap ();
}
