/* { dg-lto-do link } */
/* { dg-lto-options { { -O0 -flto -w -std=gnu17 } { -O2 -flto -w -std=gnu17 } }  } */
/* { dg-extra-ld-options { -r -nostdlib -flinker-output=nolto-rel } } */

float a;
double sin ();
void
update_filter ()
{
  a = sin (0);
}
