/* { dg-do compile } */
extern char *x;
extern void foo (void);
void f (char *s, char *se, char *mp, char *y)
{
  while (s != se)
    {
      char *p;
      foo ();
      p = s + *mp;
      *y++ = *p;
      s = p;
    }

  x = s;
}
