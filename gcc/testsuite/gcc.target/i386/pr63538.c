/* PR target/63538 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mcmodel=medium -mlarge-data-threshold=0" } */

static char *str = "Hello World";

char *foo ()
{
  return str;
}
/* See PR90698 re. Darwin xfail.  */
/* { dg-final { scan-assembler "movabs" { xfail { *-*-darwin* } } } } */
