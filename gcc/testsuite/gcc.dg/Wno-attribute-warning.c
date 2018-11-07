/* { dg-do compile } */
/* { dg-options "-Werror -Wno-error=attribute-warning" } */

int f1(void) __attribute__ ((warning("Please avoid f1")));
int func1(void)
{
  return f1(); /* { dg-warning "'f1' declared with attribute warning: Please avoid f1" } */
}
