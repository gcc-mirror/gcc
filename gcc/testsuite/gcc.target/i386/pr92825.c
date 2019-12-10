/* PR middle-end/92825 */
/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-O2 -fstack-protector-strong" } */
/* { dg-final { scan-assembler-not "__stack_chk_fail" } } */

int
foo (int r, int g, int b)
{
  union U { int rgba; char p[4]; } u;
  u.p[0] = r;
  u.p[1] = g;
  u.p[2] = b;
  u.p[3] = -1;
  return u.rgba;
}
