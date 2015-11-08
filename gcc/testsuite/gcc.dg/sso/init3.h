struct __attribute__((scalar_storage_order("little-endian"), packed)) Nested1
{
  unsigned C1 : 7;
  unsigned C2 : 7;
  unsigned C3 : 7;
  unsigned B  : 3;
};

struct __attribute__((scalar_storage_order("little-endian"), packed)) R1
{
  unsigned S1 : 6;
  unsigned I  : 32;
  unsigned S2 : 2;
  struct Nested1 N;
};

struct __attribute__((scalar_storage_order("big-endian"), packed)) Nested2
{
  unsigned C1 : 7;
  unsigned C2 : 7;
  unsigned C3 : 7;
  unsigned B  : 3;
};

struct __attribute__((scalar_storage_order("big-endian"), packed)) R2
{
  unsigned S1 : 6;
  unsigned I  : 32;
  unsigned S2 : 2;
  struct Nested2 N;
};

struct R1 My_R1 = { 2, 0x78ABCDEF, 1, { 0x12, 0x34, 0x56, 4 } };
struct R2 My_R2 = { 2, 0x78ABCDEF, 1, { 0x12, 0x34, 0x56, 4 } };
