/* PR c/84721 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

int a[({ int b })];		/* { dg-error "braced-group within expression allowed only inside a function" } */
int c[({ int d () {}; })];	/* { dg-error "braced-group within expression allowed only inside a function" } */
