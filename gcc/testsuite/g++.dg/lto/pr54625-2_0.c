/* { dg-lto-do link } */
/* { dg-extra-ld-options { -r -nostdlib } } */

float a;
double sin ();
update_filter ()
{
  a = sin (0);
}
