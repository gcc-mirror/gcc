/* { dg-do run } */
/* { dg-options "-O" } */
/* { dg-options "-O -march=i686" { target { { i686-*-* x86_64-*-* } && ia32 } } } */

extern void abort(void);

float foo(float f)
{
  if (f < 0.0f)
    f = -f;

  return f;
}

int main(void)
{
  if (foo (-1.0f) != 1.0f)
    abort ();

  return 0;
}
