/* PR c/35742 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-bogus "not supported by" "" { target *-*-* } 0 } */

void
foo ()
{
  for (;;)
    ({break;})();	/* { dg-error "is not a function" } */
  for (;;)
    ({continue;})();	/* { dg-error "is not a function" } */
}
