/* PR rtl-optimization/51495 */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

void bar (void);

int
foo (int i)
{
  static const void *const table[] = { &&begin, &&end };
  goto *(table[i]);
begin:
  bar ();
end:
  return 0;
}
