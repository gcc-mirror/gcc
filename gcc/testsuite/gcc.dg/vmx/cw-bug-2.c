#include <altivec.h>
#include <stdlib.h>

int main(void)
{
  if (vec_any_ne((vector unsigned short)(((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})),
		 vec_mfvscr()))
    abort ();
}
