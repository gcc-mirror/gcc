/* PR target/90405 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-Ofast -mthumb -mtpcs-leaf-frame" } */


#define vector __attribute((vector_size(16)))
vector float a;

float
foo (float b)
{
  vector float c = { 0, 0, 0, 0 };
  vector float d = { 0, 0, 0, 0 };
  d += c;
  return ((float *)&c)[2];
}

float
bar (vector float a, int b, vector float c)
{
  vector float e = c * a;
  a = (vector float) { 0, 0, 0, 0 };
  c = (vector float) { 0, 0, 0, 0 };
  float d = ((float *)&a)[0];
  float f = ((float *)&c)[0];
  return d * f;
}
