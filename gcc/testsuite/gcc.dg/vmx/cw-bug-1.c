#include <altivec.h>
#include <stdlib.h>

#define ZERO (((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}))

int main(void)
{
  vector unsigned char a = ZERO;
  if (vec_any_ne(a, ZERO))
    abort ();
  return 0;
}
