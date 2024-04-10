/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fdbg-cnt=vect_loop:1" } */

_BitInt(837) g, h;

void
fn1(void)
{
  for (; g; g++)
    for (; h; h++)
      ;
}
/* { dg-message "dbgcnt" "" { target *-*-* } 0 } */
