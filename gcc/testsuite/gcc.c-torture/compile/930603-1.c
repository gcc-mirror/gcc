union u { union u *a; double d; };
union u *s, g();

f()
{
  union u x = g();

  s[0] = *x.a;
  s[1] = g();
}
