__complex__ float
f(__complex__ float x)
{
  __complex__ float res;

  if (__real__ x == 0.0)
    res = x;
  return res;
}
