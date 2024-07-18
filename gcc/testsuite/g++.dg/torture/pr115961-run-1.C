/* PR target/115961 */
/* { dg-do run } */

struct e
{
  unsigned pre : 12;
  unsigned a : 4;
};

static unsigned min_u (unsigned a, unsigned b)
{
  return (b < a) ? b : a;
}

__attribute__((noipa))
void bug (e * v, unsigned def, unsigned use) {
  e & defE = *v;
  defE.a = min_u (use + 1, 0xf);
}

__attribute__((noipa, optimize(0)))
int main(void)
{
  e v = { 0xded, 3 };

  bug(&v, 32, 33);

  if (v.a != 0xf)
    __builtin_abort ();

  return 0;
}
