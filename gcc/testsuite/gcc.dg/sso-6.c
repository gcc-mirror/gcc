/* Test support of scalar_storage_order pragma */

/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

#pragma scalar_storage_order /* { dg-warning "missing .big-endian., .little-endian., or .default." } */

#pragma scalar_storage_order big-endian

struct S1
{
  int i;
};

struct __attribute__((scalar_storage_order("little-endian"))) S2
{
  int i;
};

#pragma scalar_storage_order little-endian

struct S3
{
  int i;
};

struct __attribute__((scalar_storage_order("big-endian"))) S4
{
  int i;
};

#pragma scalar_storage_order default

struct S5
{
  int i;
};

#pragma scalar_storage_order other /* { dg-warning "expected .big-endian., .little-endian., or .default." } */

struct S1 my_s1 = { 0x12345678 };
struct S2 my_s2 = { 0x12345678 };
struct S3 my_s3 = { 0x12345678 };
struct S4 my_s4 = { 0x12345678 };
struct S5 my_s5 = { 0x12345678 };

unsigned char big_endian_pattern[4] = { 0x12, 0x34, 0x56, 0x78 };
unsigned char little_endian_pattern[4] = { 0x78, 0x56, 0x34, 0x12 };

int main (void)
{
  if (__builtin_memcmp (&my_s1, &big_endian_pattern, 4) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (&my_s2, &little_endian_pattern, 4) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (&my_s3, &little_endian_pattern, 4) != 0)
    __builtin_abort ();

  if (__builtin_memcmp (&my_s4, &big_endian_pattern, 4) != 0)
    __builtin_abort ();

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if (__builtin_memcmp (&my_s5, &little_endian_pattern, 4) != 0)
    __builtin_abort ();
#else
  if (__builtin_memcmp (&my_s5, &big_endian_pattern, 4) != 0)
    __builtin_abort ();
#endif

  return 0;
}
