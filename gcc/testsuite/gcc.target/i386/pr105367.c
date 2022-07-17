/* PR target/105367 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mveclibabi=acml" } */

_Float64 g;

void
foo (void)
{
  _Float64 f = __builtin_sin (g);
  g = __builtin_fmax (__builtin_sin (f), f);
}
