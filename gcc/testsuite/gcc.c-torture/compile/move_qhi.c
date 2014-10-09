move (a, b)
     char a, b;
{
  char s;
  s = a;
  if (s)
    gurka (s);
  foo (b, a);
  a = bar ();
  b = bar ();
  gra (s);
}
