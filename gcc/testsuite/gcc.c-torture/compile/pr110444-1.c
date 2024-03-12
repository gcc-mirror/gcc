/* This used to ICE after SLP during match-and-simplify
   as real_can_shorten_arithmetic was called with the vector
   mode.  */
void f(float *a, float *b, float *c, int size)
{
  float t[2];
  t[0] = b[0] - (float)__builtin_pow(c[0], 2);
  t[1] = b[1] - (float)__builtin_pow(c[1], 2);
  a[0] = t[0];
  a[1] = t[1];
}
