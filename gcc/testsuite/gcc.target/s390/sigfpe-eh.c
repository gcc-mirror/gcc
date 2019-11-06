/* { dg-do compile } */
/* { dg-options "-march=z196 -O2 -fexceptions -fnon-call-exceptions" } */

extern float f (void);
extern float g (void);

float h (float x, float y)
{
  return x < y ? f () : g ();
}
