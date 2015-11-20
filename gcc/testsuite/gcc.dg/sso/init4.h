#define Pi 3.1415927f

struct __attribute__((scalar_storage_order("little-endian"))) R1
{
  float F;
};

struct __attribute__((scalar_storage_order("big-endian"))) R2
{
  float F;
};

struct R1 My_R1 = { Pi };
struct R2 My_R2 = { Pi };
