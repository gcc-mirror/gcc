/* { dg-do run } */
/* { dg-options "-fsso-struct=native" } */
/* { dg-require-effective-target int32plus } */

struct S1
{
  int i;
};


struct S1 my_s1 = { 0x12345678 };

unsigned char big_endian_pattern[4] = { 0x12, 0x34, 0x56, 0x78 };
unsigned char little_endian_pattern[4] = { 0x78, 0x56, 0x34, 0x12 };

int main (void)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (__builtin_memcmp (&my_s1, &little_endian_pattern, 4) != 0)
    __builtin_abort ();
#else
  if (__builtin_memcmp (&my_s1, &big_endian_pattern, 4) != 0)
    __builtin_abort ();
#endif

  return 0;
}
