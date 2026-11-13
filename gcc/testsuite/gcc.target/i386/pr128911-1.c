/* PR target/128911 */
/* { dg-do run { target { { ! ia32 } && { ! *-*-darwin* } } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-O1 -mapxf -fno-tree-fre -fno-tree-dominator-opts -fno-tree-sra" } */

__attribute__((noipa)) int
f (void)
{
  unsigned int m[2];
  m[0] = 8;
  m[1] = 48;
  unsigned int a = m[0], b = m[1];
  return (b > 175) | (a > 47);
}

int
main ()
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  if (f () != 0)
    __builtin_abort ();
  return 0;
}
