/* Test C23 reproducible attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a () [[reproducible()]]; /* { dg-error "'reproducible' attribute does not take any arguments" } */

int b () [[reproducible(0)]]; /* { dg-error "expected" } */
			      /* { dg-error "'reproducible' attribute does not take any arguments" "" { target *-*-* } .-1 } */

int c () [[reproducible("", 123)]]; /* { dg-error "expected" } */
				    /* { dg-error "'reproducible' attribute does not take any arguments" "" { target *-*-* } .-1 } */

int d () [[reproducible((""))]]; /* { dg-error "expected" } */
				 /* { dg-error "'reproducible' attribute does not take any arguments" "" { target *-*-* } .-1 } */
