/* PR target/92841 */
/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-O2 -fstack-protector-strong -masm=att" } */
/* { dg-final { scan-assembler-not "xor\[lq]\t%(\[re]\[a-z0-9]*), %\\1\[\n\r]*\tmov\[lq]\t\[^\n\r]*, %\\1" } } */

const struct S { int b; } c[] = {30, 12, 20, 0, 11};
void bar (int *);

void
foo (void)
{
  int e[4];
  const struct S *a;
  for (a = c; a < c + sizeof (c) / sizeof (c[0]); a++)
    if (a->b)
      bar (e);
}
