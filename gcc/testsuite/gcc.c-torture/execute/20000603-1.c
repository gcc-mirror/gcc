/* It is not clear whether this test is conforming.  See DR#236
   http://www.open-std.org/jtc1/sc22/wg14/www/docs/dr_236.htm.  However,
   there seems to be consensus that the presence of a union to aggregate
   struct s1 and struct s2 should make it conforming.  */
struct s1 { double d; };
struct s2 { double d; };
union u { struct s1 x; struct s2 y; };

double f(struct s1 *a, struct s2 *b)
{
  a->d = 1.0;
  return b->d + 1.0;
}

int main()
{
  union u a;
  a.x.d = 0.0;
  if (f (&a.x, &a.y) != 2.0)
    abort ();
  return 0;
}
