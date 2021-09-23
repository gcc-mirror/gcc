/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int bar();
__GIMPLE
int foo()
{
  if (bar()) /* { dg-error "comparison required" } */
    goto bb1;
  else
    goto bb2;
}
