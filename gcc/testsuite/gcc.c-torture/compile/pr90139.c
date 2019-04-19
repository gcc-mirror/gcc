/* PR middle-end/90139 */

typedef float __attribute__((vector_size (sizeof (float)))) V;
void bar (int, V *);
int l;

void
foo (void)
{
  V n, b, o;
  while (1)
    switch (l)
      {
      case 0:
	o = n;
	n = b;
	b = o;
	bar (1, &o);
      }
}
