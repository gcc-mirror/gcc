/* This used to ICE in SRA as SRA got
   confused by the zero signed assigment. */

struct empty {};
struct s { int i; };
struct z
{
  int j;
  struct empty e;
  struct s s;
  int k;
};

void bar (struct z);
void baz (int);

void foo (void)
{
  struct z z, z2;

  z.k = 8;
  z2.s.i = 1;
  z = z2;
  bar (z);
  z.e = (struct empty) {};
  baz (z.k);
}
