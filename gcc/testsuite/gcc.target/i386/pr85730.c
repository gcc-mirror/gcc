/* PR target/85730 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse4" } */

typedef char V __attribute__((vector_size(4)));

V
test_and (V v, char c)
{
  v[0] &= c;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]andb" } } */

V
test_or (V v, char c)
{
  v[0] |= c;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]orb" } } */

V
test_xor (V v, char c)
{
  v[0] ^= c;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]xorb" } } */

V
test_not (V v)
{
  v[0] = ~v[0];

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]notb" } } */

V
test_sal (V v)
{
  v[0] <<= 3;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]salb" } } */

V
test_sar (V v)
{
  v[0] >>= 3;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]sarb" } } */

V
test_add (V v, char c)
{
  v[0] += c;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]addb" } } */

V
test_sub (V v, char c)
{
  v[0] -= c;

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]subb" } } */

V
test_neg (V v)
{
  v[0] = -v[0];

  return v;
}

/* { dg-final { scan-assembler "\[ \t\]negb" } } */
