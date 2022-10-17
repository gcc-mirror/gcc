float
test_1 (int *flag, float theta)
{
  float t;
  float f;

  if (*flag)
    t = 2.0f;

  f = __builtin_sinf (theta);
  
  if (*flag)
    f *= t;

  return f;
}
