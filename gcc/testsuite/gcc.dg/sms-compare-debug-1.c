/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -fmodulo-sched --param sms-min-sc=1" } */

int a, c, e, f, g;
void
h (int i)
{
  a = i;
}
void
j (char *i, long k)
{
  while (k--)
    c = *i++;
}
void
l (unsigned char *i, long k)
{
  unsigned char *b = i + k;
  while (i < b)
    {
      h (*i);
      i++;
    }
}
void
m ()
{
  while (e)
    {
      float d = g;
      l ((char *) &d, sizeof (g));
      if (f)
	j ((char *) &d, sizeof (g));
    }
}
