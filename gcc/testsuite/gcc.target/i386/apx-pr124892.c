/* PR target/124892 */
/* { dg-do run { target { apxf && { lzcnt && { { ! ia32 } && { ! *-*-darwin* } } } } } } */
/* { dg-options "-O2 -frename-registers" } */

[[gnu::noipa, gnu::target ("apxf"), gnu::target ("lzcnt")]] char
foo (unsigned u)
{
  return __builtin_stdc_bit_ceil (u);
}

int
main ()
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;
  if (!__builtin_cpu_supports ("lzcnt"))
    return 0;
  if (foo (7) != 8)
    __builtin_abort ();
}
