/* PR target/98959 */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target lp64 } */  /* for -mcmodel= */
/* { dg-options "-fno-schedule-insns -O2 -mcmodel=small" } */

/* Verify we do not ICE on the following.  */

typedef __attribute__ ((altivec (vector__))) unsigned __int128 v1ti_t;

v1ti_t foo (v1ti_t v);

void
bug ()
{
  v1ti_t dv = { ((31415926539) << 6) };
  dv = foo (dv);
  if (dv[0] != 0)
    __builtin_abort ();
}
