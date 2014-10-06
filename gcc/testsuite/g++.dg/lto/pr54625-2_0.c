/* { dg-lto-do link } */
/* { dg-extra-ld-options { -r -nostdlib } } */

float a;
double sin ();
void
update_filter ()
{
  a = sin (0);
}
