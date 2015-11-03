/* { dg-do run { target { *-*-linux* } } } */
/* { dg-options "-fstack-check" } */
/* { dg-skip-if "" { arm_thumb1 } } */

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
      j[48] = 0;
    }

  return !p[0];
}
