#include <altivec.h>
static vector unsigned int v = {0x01020304,0x05060708,0x21324354,0x65768798};
static vector unsigned int f() { return vec_splat(v,0); }
int main() {
  vector unsigned int x = f();
  return 0;
}
