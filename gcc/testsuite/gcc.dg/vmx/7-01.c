/* { dg-do compile } */
#include <altivec.h>
extern vector signed short image[];
extern vector signed short band[];

#define load(a,b) (a[b])
#define store(v,a,b) (a[b]) = (v)

void
haar (vector signed char a, vector signed char b, vector signed char c,
      vector signed char d, unsigned int N, int XX)
{
  unsigned int i;
  vector unsigned char high, low;
  vector signed int zero = ((vector signed int){0,0,0,0});

  for (i = 0; i < N; i++) {
    high = (vector unsigned char) (vec_vmrghh (load(image, i+XX),
					       load(image, i)));
    low = (vector unsigned char) (vec_vmrglh (load(image, i+XX),
					      load(image, i)));

    store (vec_vpkswss (vec_vmsummbm (a, high, zero),
			vec_vmsummbm (a, low, zero)),
	   band, i);
    store (vec_vpkswss (vec_vmsummbm (b, high, zero),
			vec_vmsummbm (b, low, zero)),
	   band, i+1);
    store(vec_vpkswss (vec_vmsummbm (c, high, zero),
		       vec_vmsummbm (c, low, zero)),
	  band, i+2);
    store(vec_vpkswss (vec_vmsummbm (d, high, zero),
		       vec_vmsummbm (d, low, zero)),
	  band, i+3);
  }
}
