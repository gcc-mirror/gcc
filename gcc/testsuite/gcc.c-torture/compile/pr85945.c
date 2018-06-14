/* PR target/85945 */

typedef float V __attribute__((vector_size(16)));
union U { V v; float f[4]; };
int f;
float g[4];

void
foo (void)
{
  V d;
  union U i;
  i.v = d;
  for (f = 0; f < 4; f++)
    g[f] = i.f[f];
}
