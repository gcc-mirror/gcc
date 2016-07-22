/* Test support of scalar_storage_order pragma */

/* { dg-do run } */
/* { dg-options "-fsso-struct=big-endian" } */
/* { dg-require-effective-target int32plus } */

struct S1
{
  int i;
};

#pragma scalar_storage_order little-endian

struct S2
{
  int i;
};

#pragma scalar_storage_order default

struct S3
{
  int i;
};

struct S1 my_s1 = { 0x12345678 };
struct S2 my_s2 = { 0x12345678 };
struct S3 my_s3 = { 0x12345678 };

unsigned char big_endian_pattern[4] = { 0x12, 0x34, 0x56, 0x78 };
unsigned char little_endian_pattern[4] = { 0x78, 0x56, 0x34, 0x12 };

int main (void)
{
  if (__builtin_memcmp (&my_s1, &big_endian_pattern, 4) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (&my_s2, &little_endian_pattern, 4) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (&my_s3, &big_endian_pattern, 4) != 0)
    __builtin_abort ();

  return 0;
}
