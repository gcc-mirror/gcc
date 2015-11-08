struct __attribute__((scalar_storage_order("big-endian"))) Nested1
{
  int A[3];
};

struct __attribute__((scalar_storage_order("little-endian"))) R1
{
  int I;
  struct Nested1 N;
};

struct __attribute__((scalar_storage_order("little-endian"))) Nested2
{
  int A[3];
};

struct __attribute__((scalar_storage_order("big-endian"))) R2
{
  int I;
  struct Nested2 N;
};

struct R1 My_R1 = { 0x12345678, { { 0xAB0012, 0xCD0034, 0xEF0056 } } };
struct R2 My_R2 = { 0x12345678, { { 0xAB0012, 0xCD0034, 0xEF0056 } } };
