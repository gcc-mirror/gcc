/* PR target/117926 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -m3dnow" } */

struct s
{
  int i[2];
  float f[2];
  double d;
};

void f (struct s *s)
{
  s->f[0] = s->i[0];
  s->f[1] = s->i[1];
}

/* { dg-final { scan-assembler-not "pi2fd" } } */
