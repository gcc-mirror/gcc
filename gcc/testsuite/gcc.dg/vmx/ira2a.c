#include <altivec.h>
vector unsigned char  *u8ptr;
int i;

int main()
{
  vec_dstst(u8ptr, i, 3U);
  return 0;
}
