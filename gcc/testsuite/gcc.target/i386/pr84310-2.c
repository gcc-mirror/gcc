/* { dg-do compile } */
/* { dg-options "-O2 -malign-loops=16" } */
/* { dg-warning "is obsolete" "" { target *-*-* } 0 } */

void
c (void)
{
  for (;;)
    ;
}
