/* PR28322: ignore unknown -Wno-* if no warning is emitted.  */
/* { dg-do compile } */
/* { dg-options " -Wno-foobar -Wno-div-by-zero" } */

void foo(void)
{
  int i =  1/0;
}
