/* { dg-do run { target { *-*-linux* } } } */
/* { dg-require-stack-check "" } */
/* { dg-options "-fstack-check" } */

int main(void)
{
  char *p;
  if (1)
    {
      char i[48];
      p = __builtin_alloca(8);
      p[0] = 1;
    }

  if (1)
    {
      char i[48], j[64];
      j[32] = 0;
    }

  return !p[0];
}
