// PR tree-optimization/107369
// { dg-do compile { target c++11 } }
// { dg-options "-O1" }

void
foo (int x)
{
  if (x == 1)
    goto l1;			// { dg-message "from here" }

  [[assume (({ l1:; 1; }))]];	// { dg-error "jump to label 'l1'" }
}				// { dg-message "enters statement expression" "" { target *-*-* } .-1 }
