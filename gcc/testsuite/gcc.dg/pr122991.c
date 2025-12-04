/* PR target/122991 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse4" { target i?86-*-* x86_64-*-* } } */

int
foo ()
{
  return __builtin_rev_crc32_data32 (0, 0, 0);
}

int
bar ()
{
  return __builtin_rev_crc32_data32 (-1U, -1U, -1U);
}

int
baz ()
{
  return __builtin_crc32_data32 (0, 0, 0);
}

int
qux ()
{
  return __builtin_crc32_data32 (-1U, -1U, -1U);
}
