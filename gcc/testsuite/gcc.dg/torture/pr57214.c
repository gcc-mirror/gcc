/* { dg-do compile } */

extern int baz (void);
extern int foo (void) __attribute__ ((returns_twice));

void
bar (_Bool b)
{
  int buf[1];
  while (1)
    {
      _Bool x = 1;
      if (b)
	baz ();
      b = 1;
      baz ();
      x = 0;
      unsigned int i;
      while (buf[i] && i)
	i++;
      foo ();
      if (!x)
	b = 0;
    }
}
