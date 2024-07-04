/* { dg-do compile } */
/* { dg-options "-fstrub=relaxed" } */
/* { dg-require-effective-target strub } */

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
