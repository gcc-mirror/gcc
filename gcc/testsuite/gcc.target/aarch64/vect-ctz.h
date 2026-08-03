/* Shared source for the Advanced SIMD and the SVE ctz code-quality tests
   and for the execution test.  */

typedef __UINT8_TYPE__ u8;
typedef __UINT16_TYPE__ u16;

/* The OR keeps the input nonzero so that the loop is just the count.  */
__attribute__((noipa)) void
ctzb (u8 *__restrict d, u8 *__restrict a)
{
  for (int i = 0; i < 16; i++)
    d[i] = __builtin_ctzg ((u8) (a[i] | 0x80));
}

__attribute__((noipa)) void
ctzb_n (u8 *__restrict d, u8 *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_ctzg (a[i], 8);
}

__attribute__((noipa)) void
ctzh (u16 *__restrict d, u16 *__restrict a)
{
  for (int i = 0; i < 8; i++)
    d[i] = __builtin_ctzg ((u16) (a[i] | 0x8000));
}

__attribute__((noipa)) void
ctzh_n (u16 *__restrict d, u16 *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_ctzg (a[i], 16);
}
