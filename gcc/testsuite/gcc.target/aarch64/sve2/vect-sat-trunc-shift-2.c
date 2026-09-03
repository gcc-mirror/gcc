/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only -fdump-tree-vect-details" } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;

/* A variable shift amount has no immediate form, so the shift stays
   separate from the saturating narrowing.  */

void __attribute__((noipa))
h_to_b (uint8_t *__restrict out, const uint16_t *__restrict in,
	const uint16_t *__restrict shifts, int n)
{
  for (int i = 0; i < n; ++i)
    {
      uint16_t x = in[i] >> (shifts[i] & 15);
      out[i] = x > 255 ? 255 : x;
    }
}

void __attribute__((noipa))
s_to_h (uint16_t *__restrict out, const uint32_t *__restrict in,
	const uint32_t *__restrict shifts, int n)
{
  for (int i = 0; i < n; ++i)
    {
      uint32_t x = in[i] >> (shifts[i] & 31);
      out[i] = x > 65535 ? 65535 : x;
    }
}

/* { dg-final { scan-assembler-not {\t[su]qshrnb\t} } } */
/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.b, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.h, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 2 "vect" } } */
