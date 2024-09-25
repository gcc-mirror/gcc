/* Test C23 unsequenced attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a () [[unsequenced()]]; /* { dg-error "'unsequenced' attribute does not take any arguments" } */

int b () [[unsequenced(0)]]; /* { dg-error "expected" } */
			     /* { dg-error "'unsequenced' attribute does not take any arguments" "" { target *-*-* } .-1 } */

int c () [[unsequenced("", 123)]]; /* { dg-error "expected" } */
				   /* { dg-error "'unsequenced' attribute does not take any arguments" "" { target *-*-* } .-1 } */

int d () [[unsequenced((""))]]; /* { dg-error "expected" } */
				/* { dg-error "'unsequenced' attribute does not take any arguments" "" { target *-*-* } .-1 } */
