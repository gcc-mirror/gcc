typedef int int32_t __attribute__((mode (__SI__)));

struct S
{
  static int32_t i;
  static void set (int32_t ii) { i = -ii; }
};
