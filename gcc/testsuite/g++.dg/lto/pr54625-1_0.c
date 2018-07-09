/* { dg-lto-do link } */
/* { dg-extra-ld-options { -r -nostdlib -flinker-output=nolto-rel } } */

float a;
double sin ();
void
speex_resampler_init_frac ()
{
  a = sin (0);
}

