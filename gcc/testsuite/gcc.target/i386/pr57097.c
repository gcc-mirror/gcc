/* { dg-do compile } */
/* { dg-options "-O2 -fPIC" } */
extern double ad[], bd[], cd[], dd[];
extern long long all[], bll[], cll[], dll[];

int
main (int i, char **a)
{
  bd[i] = i + 64;
  if (i % 3 == 0)
    {
      cd[i] = i;
    }
  dd[i] = i / 2;
  ad[i] = i * 2;
  if (i % 3 == 1)
    {
      dll[i] = 127;
    }
  dll[i] = i;
  cll[i] = i * 2;
  switch (i % 3)
    {
    case 0:
      bll[i] = i + 64;
    }
  all[i] = i / 2;
  return 0;
}
