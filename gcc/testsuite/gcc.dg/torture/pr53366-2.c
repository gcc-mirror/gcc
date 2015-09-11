/* PR tree-optimization/53366 */
/* { dg-do run } */

extern void abort (void);

struct T { float r[3], i[3]; };
struct U { struct T j[4]; };

void __attribute__ ((noinline))
foo (struct U *__restrict y, const float _Complex *__restrict x)
{
  int i, j;
  for (j = 0; j < 4; ++j)
    {
      float a = __real__ x[j];
      float b = __imag__ x[j];
      float c = __real__ x[j + 4];
      float d = __imag__ x[j + 4];
      for (i = 0; i < 3; ++i)
        {
          y->j[j].r[i] = y->j[j].r[i] + a + c;
          y->j[j].i[i] = y->j[j].i[i] + b + d;
        }
    }
}

_Complex float x[8];
struct U y;

int
main ()
{
  int i, j;
  for (i = 0; i < 8; ++i)
    {
      x[i] = i + 1.0iF * (2 * i);
      __asm__ volatile ("");
    }
  foo (&y, x);
  for (j = 0; j < 4; ++j)
    for (i = 0; i < 3; ++i)
      if (y.j[j].r[i] != __real__ (x[j] + x[j + 4])
          || y.j[j].i[i] != __imag__ (x[j] + x[j + 4]))
        __builtin_abort ();
  return 0;
}
