/* { dg-lto-do link } */
/* { dg-extra-ld-options { -r -nostdlib } } */

float a;
double sin ();
speex_resampler_init_frac ()
{
  a = sin (0);
}

