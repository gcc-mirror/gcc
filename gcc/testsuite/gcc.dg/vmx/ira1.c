#include <altivec.h>
#include <stdlib.h>
vector unsigned char u8a, u8b;

int main()
{
  if (!vec_all_eq(u8a, u8b))
    abort ();
  return 0;
}
