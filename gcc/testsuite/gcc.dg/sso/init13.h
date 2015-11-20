#define I (__extension__ 1.0iF)
#define Pi 3.1415927f

struct __attribute__((scalar_storage_order("little-endian"))) R1
{
  _Complex float F;
};

struct __attribute__((scalar_storage_order("big-endian"))) R2
{
  _Complex float F;
};

struct R1 My_R1 = { Pi - Pi * I };
struct R2 My_R2 = { Pi - Pi * I };
