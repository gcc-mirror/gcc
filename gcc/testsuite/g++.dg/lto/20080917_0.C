// { dg-lto-do assemble }
// { dg-lto-options {{-O2 -flto -funsigned-char}} }
int
foo (char *s, int flag)
{
  for (;;)
    {
      unsigned char c;
      if (flag)
	c = *s;
      else
	c = *s;
      return c;
    }
}

int
baz (const char *s, int flag)
{
  for (;;)
    {
      unsigned char c;
      if (flag)
	c = *s;
      else
	c = *s;
      return c;
    }
}
