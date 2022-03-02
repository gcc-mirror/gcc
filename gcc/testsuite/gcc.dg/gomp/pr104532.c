/* PR c/104532 */
/* { dg-do compile } */

void
foo (int x)
{
  #pragma omp target enter data map (to: x->vectors)	/* { dg-error "invalid type argument of '->'" } */
}							/* { dg-error "must contain at least one" "" { target *-*-* } .-1 } */

void
bar (int x)
{
  #pragma omp target enter data map (to: x->vectors[])	/* { dg-error "invalid type argument of '->'" } */
}							/* { dg-error "must contain at least one" "" { target *-*-* } .-1 } */
                                                        /* { dg-error "expected expression before" "" { target *-*-* } .-2 } */
