#include <altivec.h>
vector unsigned char v;
typedef unsigned char T[16];
T t;
typedef struct { unsigned char a[16]; } R;
R r;
typedef union {
  unsigned char u8[16];
  signed char s8[16];
  unsigned short u16[8];
  signed short s16[8];
  unsigned int u32[4];
  signed int s32[4];
  float f32[4];
} U;
U u;
static void use(void *p) {
}
int main() {
  use (&v);
  use (&t);
  use (&r);
  use (&u);
  return 0;
}

