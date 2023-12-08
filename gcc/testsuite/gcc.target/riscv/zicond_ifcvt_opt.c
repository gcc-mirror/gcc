/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb_zicond -mabi=lp64d -O2 " } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Os" "-Og" "-O3" "-Oz" "-flto"} } */

long
test_ADD_ceqz (long x, long y, long z, long c)
{
  if (c)
    x = y + z;
  else
    x = y;
  return x;
}

long
test_ADD_ceqz_x (long x, long z, long c)
{
  if (c)
    x = x + z;

  return x;
}

long
test_ADD_nez (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = y + z;
  return x;
}

long
test_ADD_nez_x (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = x + z;
  return x;
}

long
test_ADD_nez_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y + z;
  else
    x = y;
  return x;
}

long
test_ADD_nez_x_2 (long x, long z, long c)
{
  if (!c)
    x = x + z;

  return x;
}

long
test_ADD_eqz_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = y + z;
  return x;
}

long
test_ADD_eqz_x_2 (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = x + z;
  return x;
}

long
test_SUB_ceqz (long x, long y, long z, long c)
{
  if (c)
    x = y - z;
  else
    x = y;
  return x;
}

long
test_SUB_ceqz_x (long x, long z, long c)
{
  if (c)
    x = x - z;

  return x;
}

long
test_SUB_nez (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = y - z;
  return x;
}

long
test_SUB_nez_x (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = x - z;
  return x;
}

long
test_SUB_nez_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y - z;
  else
    x = y;
  return x;
}

long
test_SUB_nez_x_2 (long x, long z, long c)
{
  if (!c)
    x = x - z;

  return x;
}

long
test_SUB_eqz_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = y - z;
  return x;
}

long
test_SUB_eqz_x_2 (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = x - z;
  return x;
}

long
test_IOR_ceqz (long x, long y, long z, long c)
{
  if (c)
    x = y | z;
  else
    x = y;
  return x;
}

long
test_IOR_ceqz_x (long x, long z, long c)
{
  if (c)
    x = x | z;

  return x;
}

long
test_IOR_nez (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = y | z;
  return x;
}

long
test_IOR_nez_x (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = x | z;
  return x;
}

long
test_IOR_nez_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y | z;
  else
    x = y;
  return x;
}

long
test_IOR_nez_x_2 (long x, long z, long c)
{
  if (!c)
    x = x | z;

  return x;
}

long
test_IOR_eqz_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = y | z;
  return x;
}

long
test_IOR_eqz_x_2 (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = x | z;
  return x;
}

long
test_XOR_ceqz (long x, long y, long z, long c)
{
  if (c)
    x = y ^ z;
  else
    x = y;
  return x;
}

long
test_XOR_ceqz_x (long x, long z, long c)
{
  if (c)
    x = x ^ z;

  return x;
}

long
test_XOR_nez (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = y ^ z;
  return x;
}

long
test_XOR_nez_x (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = x ^ z;
  return x;
}

long
test_XOR_nez_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y ^ z;
  else
    x = y;
  return x;
}

long
test_XOR_nez_x_2 (long x, long z, long c)
{
  if (!c)
    x = x ^ z;

  return x;
}

long
test_XOR_eqz_2 (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = y ^ z;
  return x;
}

long
test_XOR_eqz_x_2 (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = x ^ z;
  return x;
}

long
test_ADD_ceqz_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = z + y;
  else
    x = y;
  return x;
}

long
test_ADD_ceqz_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    x = z + x;

  return x;
}

long
test_ADD_nez_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = z + y;
  return x;
}

long
test_ADD_nez_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = z + x;
  return x;
}

long
test_ADD_nez_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = z + y;
  else
    x = y;
  return x;
}

long
test_ADD_nez_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    x = z + x;

  return x;
}

long
test_ADD_eqz_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = z + y;
  return x;
}

long
test_ADD_eqz_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = z + x;
  return x;
}

long
test_IOR_ceqz_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = z | y;
  else
    x = y;
  return x;
}

long
test_IOR_ceqz_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    x = z | x;

  return x;
}

long
test_IOR_nez_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = z | y;
  return x;
}

long
test_IOR_nez_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = z | x;
  return x;
}

long
test_IOR_nez_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = z | y;
  else
    x = y;
  return x;
}

long
test_IOR_nez_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    x = z | x;

  return x;
}

long
test_IOR_eqz_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = z | y;
  return x;
}

long
test_IOR_eqz_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = z | x;
  return x;
}

long
test_XOR_ceqz_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = z ^ y;
  else
    x = y;
  return x;
}

long
test_XOR_ceqz_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    x = z ^ x;

  return x;
}

long
test_XOR_nez_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (c)
    x = y;
  else
    x = z ^ y;
  return x;
}

long
test_XOR_nez_x_reverse_bin_oprands (long x, long z, long c)
{
  if (c)
    {
    }
  else
    x = z ^ x;
  return x;
}

long
test_XOR_nez_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = z ^ y;
  else
    x = y;
  return x;
}

long
test_XOR_nez_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    x = z ^ x;

  return x;
}

long
test_XOR_eqz_2_reverse_bin_oprands (long x, long y, long z, long c)
{
  if (!c)
    x = y;
  else
    x = z ^ y;
  return x;
}

long
test_XOR_eqz_x_2_reverse_bin_oprands (long x, long z, long c)
{
  if (!c)
    {
    }
  else
    x = z ^ x;
  return x;
}

/* { dg-final { scan-assembler-times {czero\.eqz} 28 } } */
/* { dg-final { scan-assembler-times {czero\.nez} 28 } } */
