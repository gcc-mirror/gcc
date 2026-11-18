/* PR target/128836 */
/* { dg-do run } */
/* { dg-options "-Os" } */

__attribute__((noipa)) float
f(int p, float x)
{
  if (p == 1)
    {
      unsigned int u;
      __builtin_memcpy (&u, &x, sizeof (u));
      u &= 0xFFFF0000u;
      __builtin_memcpy (&x, &u, sizeof (u));
      return x;
    }
  return p == 2 ? -x : x;
}

int
main ()
{
  volatile int p = 1;
  volatile float v = -87.3f;
  float r = f (p, v);
  if (r != -87.0f)
    __builtin_abort ();
  return 0;
}
