#define Pi 3.14159265358979323846

struct __attribute__((scalar_storage_order("little-endian"))) R1
{
  double F;
};

struct __attribute__((scalar_storage_order("big-endian"))) R2
{
  double F;
};

struct R1 My_R1 = { Pi };
struct R2 My_R2 = { Pi };
