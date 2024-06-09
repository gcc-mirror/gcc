/* PR analyzer/113505 */
/* { dg-additional-options "-O -fdump-analyzer" } */

enum E **foo () __attribute__((__const__));
char a[2];
void bar (char *);

void
baz (void)
{
  char *s, *l;
  for (;;)
    {
      bar (a);
      s = a;
      while (foo ()[*s])
	s++;
      l = s;
      *l++ = '\0';
      while (foo ()[*l])
	l++;
      bar (s);
    }
}
