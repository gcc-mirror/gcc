/* PR regression/21897 */

extern void abort (void);

typedef short v4hi __attribute__ ((vector_size (8)));
typedef float v4sf __attribute__ ((vector_size (16)));

union
{
  v4hi v;
  short s[4];
} u;

union
{
  v4sf v;
  float f[4];
} v;

void
foo (void)
{
  unsigned int i;
  for (i = 0; i < 2; i++)
    u.v += (v4hi) { 12, 14 };
  for (i = 0; i < 2; i++)
    v.v += (v4sf) { 18.0, 20.0, 22 };
}

int
main (void)
{
  foo ();
  if (u.s[0] != 24 || u.s[1] != 28 || u.s[2] || u.s[3])
    abort ();
  if (v.f[0] != 36.0 || v.f[1] != 40.0 || v.f[2] != 44.0 || v.f[3] != 0.0)
    abort ();
  return 0;
}
