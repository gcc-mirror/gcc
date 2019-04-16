/* PR target/89902 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fno-tree-coalesce-vars -mavx512bitalg" } */

void bar (void);

int
foo (long long x)
{
  x <<= (char) x;
  bar ();
  return x;
}
