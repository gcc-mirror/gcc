/* { dg-do compile } */

void bar (void);
void baz (int);
char *qux (void);
int a, b;

void
foo (int f, char *d)
{
  char *e;
  while (d)
    {
      if (f)
	if (e)
	  bar ();
      baz (e - (d + a));
      b = e - d;
      d = qux ();
    }
}
