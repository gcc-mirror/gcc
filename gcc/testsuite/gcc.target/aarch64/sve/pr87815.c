/* { dg-do compile { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3" } */
int a, b, d;
short e;

void f ()
{
  for (int i = 0; i < 8; i++)
    {
      e = b >= 2 ?: a >> b;
      d = e && b;
    }
}
