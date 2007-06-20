/* { dg-do run } */
/* { dg-options "-O2" } */

struct s
{
  long long a:12;
  long long b:12;
  long long c:40;
};

struct s s, *p = &s;

int
main ()
{
  p->a = 1;
  s.a = 0;
  s.b = 0;
  return p->a + s.b;
}
