struct __attribute__((scalar_storage_order("little-endian"))) R1
{
  int I;
};

struct __attribute__((scalar_storage_order("big-endian"))) R2
{
  int I;
};

struct R1 My_R1 = { 0x12345678 };
struct R2 My_R2 = { 0x12345678 };
