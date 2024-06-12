/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed -fbranch-probabilities" } */
/* { dg-require-effective-target strub } */

/* Same as torture/strub-inlineable1.c, but with -fbranch-probabilities, to
   check that IPA tree-profiling won't ICE.  It would when we refrained from
   running passes that would take it to IPA_SSA, but ran the pass that asserted
   for IPA_SSA.  */

inline void __attribute__ ((strub ("internal"), always_inline))
inl_int_ali (void)
{
  /* No internal wrapper, so this body ALWAYS gets inlined,
     but it cannot be called from non-strub contexts.  */
}

void
bat (void)
{
  /* Not allowed, not a strub context.  */
  inl_int_ali (); /* { dg-error "context" } */
}
