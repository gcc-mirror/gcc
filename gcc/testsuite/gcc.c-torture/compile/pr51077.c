/* PR middle-end/51077 */

struct S { unsigned char s, t[256]; };

void
foo (const struct S *x, struct S *y, int z)
{
  int i;
  for (i = 0; i < 8; i++)
    {
      const struct S *a = &x[i];
      __builtin___memcpy_chk (y->t, a->t, z, __builtin_object_size (y->t, 0));
      y = (struct S *) &y->t[z];
    }
}
