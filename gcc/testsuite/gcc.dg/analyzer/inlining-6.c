/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

static inline void
inner (void *p)
{
  __builtin_free (p);
}

static inline void
middle (void *q)
{
  inner (q);
  __builtin_free (q);  /* { dg-warning "double-'free' of 'r'" "warning" } */
}

void
outer (void *r)
{
  middle (r);
}
