/* { dg-do compile { target ia32 } } */
/* { dg-options "-march=i586 -mavx -O2" } */

extern double s1[];
extern double s2[];
extern long long e[];

void test (void)
{
  int i;

  for (i = 0; i < 2; i++)
    e[i] = !__builtin_isunordered(s1[i], s2[i]) && s1[i] != s2[i] ? -1 : 0;
}
