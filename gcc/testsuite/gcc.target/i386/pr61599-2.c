/* PR target/61599 */
/* With -mcmodel=medium, all the arrays will be treated as large data.  */
/* { dg-options "-mcmodel=medium -fdata-sections" { target lp64 } } */
/* { dg-do compile { target lp64 } } */

extern char a[];
extern char b[];
extern char c[];

int bar()
{
  return a[2] + b[16] + c[256];
}
