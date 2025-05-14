/* PR middle-end/119537 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

volatile int v;
void *bar (void *, void *);

void
foo (bool z)
{
  if (z)
    goto *&&x;				/* { dg-error "reference to label 'x' defined inside of 'assume' attribute expression from outside of the attribute" } */
					/* { dg-message "as a possible target of computed goto" "" { target c++ } .-1 } */
  [[gnu::assume (({ x: v += 1; true; }))]];/* { dg-message "'x' defined here" } */
					/* { dg-warning "jump to label 'x'" "" { target c++ } .-1 } */
					/* { dg-message "enters statement expression" "" { target c++ } .-2 } */
  [[gnu::assume (({ y: v += 1; true; }))]];/* { dg-message "'y' defined here" } */
					/* { dg-warning "jump to label 'y'" "" { target c++ } .-1 } */
  goto *bar (&&x, &&y);			/* { dg-error "reference to label 'x' defined inside of 'assume' attribute expression from outside of the attribute" } */
					/* { dg-error "reference to label 'y' defined inside of 'assume' attribute expression from outside of the attribute" "" { target *-*-* } .-1 } */
					/* { dg-message "as a possible target of computed goto" "" { target c++ } .-2 } */
					/* { dg-message "enters statement expression" "" { target c++ } .-3 } */
}
