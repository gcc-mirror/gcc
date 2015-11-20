struct __attribute__((scalar_storage_order("little-endian"), packed)) R1
{
  unsigned S1 : 2;
  unsigned I  : 32;
  unsigned S2 : 2;
  unsigned A1 : 9;
  unsigned A2 : 9;
  unsigned A3 : 9;
  unsigned B  : 1;
};

struct __attribute__((scalar_storage_order("big-endian"), packed)) R2
{
  unsigned S1 : 2;
  unsigned I  : 32;
  unsigned S2 : 2;
  unsigned A1 : 9;
  unsigned A2 : 9;
  unsigned A3 : 9;
  unsigned B  : 1;
};

struct R1 My_R1 = { 2, 0x12345678, 1, 0xAB, 0xCD, 0xEF, 1 };
struct R2 My_R2 = { 2, 0x12345678, 1, 0xAB, 0xCD, 0xEF, 1 };
