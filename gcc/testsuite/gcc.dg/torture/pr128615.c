/* { dg-do run } */

int b[1024], x[1024];

__attribute__((noipa)) void
f (int *__restrict q, const int *__restrict p)
{
  unsigned char i = 0;
  do
    *q++ = p[0] + p[1] + p[2] + p[3];
  while (p++, i++ != 255);  /* 256 iterations */
}

int
main (void)
{
  for (int k = 0; k < 1024; k++)
    x[k] = -1;
  f (x, b);
  if (x[256] != -1)
    __builtin_abort ();
  return 0;
}
