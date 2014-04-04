/* { dg-options "-maltivec=be -mabi=altivec -std=gnu99 -mno-vsx" } */

#include "harness.h"

static void test()
{
  vector unsigned char vuca = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  vector unsigned char vucb = {2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3};
  vector signed char vsca = {-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7};
  vector signed char vscb = {2,-3,2,-3,2,-3,2,-3,2,-3,2,-3,2,-3,2,-3};
  vector unsigned short vusa = {0,1,2,3,4,5,6,7};
  vector unsigned short vusb = {2,3,2,3,2,3,2,3};
  vector signed short vssa = {-4,-3,-2,-1,0,1,2,3};
  vector signed short vssb = {2,-3,2,-3,2,-3,2,-3};
  vector unsigned short vuse, vuso;
  vector signed short vsse, vsso;
  vector unsigned int vuie, vuio;
  vector signed int vsie, vsio;

  vuse = vec_mule (vuca, vucb);
  vuso = vec_mulo (vuca, vucb);
  vsse = vec_mule (vsca, vscb);
  vsso = vec_mulo (vsca, vscb);
  vuie = vec_mule (vusa, vusb);
  vuio = vec_mulo (vusa, vusb);
  vsie = vec_mule (vssa, vssb);
  vsio = vec_mulo (vssa, vssb);

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  check (vec_all_eq (vuse,
		     ((vector unsigned short){3,9,15,21,27,33,39,45})),
	 "vuse");
  check (vec_all_eq (vuso,
		     ((vector unsigned short){0,4,8,12,16,20,24,28})),
	 "vuso");
  check (vec_all_eq (vsse,
		     ((vector signed short){21,15,9,3,-3,-9,-15,-21})),
	 "vsse");
  check (vec_all_eq (vsso,
		     ((vector signed short){-16,-12,-8,-4,0,4,8,12})),
	 "vsso");
  check (vec_all_eq (vuie, ((vector unsigned int){3,9,15,21})), "vuie");
  check (vec_all_eq (vuio, ((vector unsigned int){0,4,8,12})), "vuio");
  check (vec_all_eq (vsie, ((vector signed int){9,3,-3,-9})), "vsie");
  check (vec_all_eq (vsio, ((vector signed int){-8,-4,0,4})), "vsio");
#else
  check (vec_all_eq (vuse,
		     ((vector unsigned short){0,4,8,12,16,20,24,28})),
	 "vuse");
  check (vec_all_eq (vuso,
		     ((vector unsigned short){3,9,15,21,27,33,39,45})),
	 "vuso");
  check (vec_all_eq (vsse,
		     ((vector signed short){-16,-12,-8,-4,0,4,8,12})),
	 "vsse");
  check (vec_all_eq (vsso,
		     ((vector signed short){21,15,9,3,-3,-9,-15,-21})),
	 "vsso");
  check (vec_all_eq (vuie, ((vector unsigned int){0,4,8,12})), "vuie");
  check (vec_all_eq (vuio, ((vector unsigned int){3,9,15,21})), "vuio");
  check (vec_all_eq (vsie, ((vector signed int){-8,-4,0,4})), "vsie");
  check (vec_all_eq (vsio, ((vector signed int){9,3,-3,-9})), "vsio");
#endif
}
