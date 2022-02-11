/* { dg-do run } */
/* { dg-options "" } */
#include <stdlib.h>
#define LOAD_IMM(var, val) \
        asm ("li %0, %1\n": "=r"(var): "i" (val))

#define CHECK_LI(type, val) \
  { \
    type var; \
    LOAD_IMM(var, val); \
    if (var != val) \
      abort(); \
  }

#define CHECK_LI32(val) CHECK_LI(int, val)
#define CHECK_LI64(val) CHECK_LI(long long, val)

int main()
{
  CHECK_LI32(0x8001);
  CHECK_LI32(0x1f01);
  CHECK_LI32(0x12345001);
  CHECK_LI32(0xf2345001);
#if __riscv_xlen == 64
  CHECK_LI64(0x8001ll);
  CHECK_LI64(0x1f01ll);
  CHECK_LI64(0x12345001ll);
  CHECK_LI64(0xf2345001ll);
  CHECK_LI64(0xf12345001ll);
  CHECK_LI64(0xff00ff00ff001f01ll);
  CHECK_LI64(0x7ffffffff2345001ll);
  CHECK_LI64(0x7f0f243ff2345001ll);
  CHECK_LI64(0x1234567887654321ll);
#endif
  return 0;
}
