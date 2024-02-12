/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fdbg-cnt=vect_loop:1" } */

#if __BITINT_MAXWIDTH__ >= 837
_BitInt(837) g, h;
#else
_BitInt(63) g, h;
#endif

void
fn1(void)
{
  for (; g; g++)
    for (; h; h++)
      ;
}

/* { dg-prune-output "dbgcnt:" } */
