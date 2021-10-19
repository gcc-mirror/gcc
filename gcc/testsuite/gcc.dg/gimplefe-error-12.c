/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int get_current ();

__GIMPLE
void foo()
{
  get_current()->flags; /* { dg-error "non-pointer" } */
}
