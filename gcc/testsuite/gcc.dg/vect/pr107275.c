/* { dg-do compile } */
struct st
{
  int a : 1;
};

void
foo (struct st *s, int n)
{
  for (int i = 0; i < n; ++i)
    {
      s[i].a = i;
      __asm__ __volatile__ ("":::"memory");
    }
}
