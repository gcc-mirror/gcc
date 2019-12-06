/* Test [*] diagnosed on old-style parameter declaration.  PR c/88704.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

void
f (x)
     int x[*];
{ /* { dg-error "not allowed in other than function prototype scope" } */
}
