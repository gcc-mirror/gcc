// PR c++/39028
// { dg-do compile }
// Origin: Stephan Springl <springl@bfw-online.de>

void
f ()
{
  int i;
  for (i = 0; i < 2; i++)
    {
      __label__ l;
      goto l;
      l:;
    }
  while (i++ < 5)
    {
      __label__ l;
      goto l;
      l:;
    }
  do
    {
      __label__ l;
      goto l;
      l:;
    }
  while (i++ < 8);
  if (1)
    {
      __label__ l;
      goto l;
      l:;
    }
  {
    __label__ l;
    goto l;
    l:;
  }
}
