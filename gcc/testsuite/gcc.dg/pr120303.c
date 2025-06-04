/* { dg-do compile } */
/* { dg-options "-std=c2y" } */

int t = _Generic (char(1));	/* { dg-error "before numeric constant" } */

