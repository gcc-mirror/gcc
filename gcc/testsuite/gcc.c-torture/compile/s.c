struct foo
{
  int a, b, c, d;
  double doubl;
} s1, s2;

struct foo
structret (s1, i1, i2, s2)
     struct foo s1, s2;
     int i1, i2;
{
  if (i1 != i2)
    {
      if (i1 < i2)
	return s1;
      else
	return s2;
    }
  s2.a = 11;
  s2.b = 22;
  s2.c = s1.c;
  s2.d = s1.d;
  return s2;
}
