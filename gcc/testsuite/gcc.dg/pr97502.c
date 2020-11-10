/* { dg-do compile } */
/* { dg-options "-O3" } */

extern char v[54];
void bar (char *);
void
foo (void)
{
  int i;
  char c[32];
  bar (c);
  for (i = 0; i < 32; i++)
    c[i] = c[i] && !v[i];
  bar (c);
}
