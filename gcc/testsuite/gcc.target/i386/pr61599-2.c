/* PR target/61599 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-mcmodel=medium -fdata-sections" { target lp64 } } */

/* With -mcmodel=medium, all the arrays will be treated as large data.  */

extern char a[];
extern char b[];
extern char c[];

int bar()
{
  return a[2] + b[16] + c[256];
}
