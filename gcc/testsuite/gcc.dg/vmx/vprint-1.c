/* These tests require an AltiVec aware C library with vector
   extensions for printf (%v).  Currently newlib handles AltiVec
   vector extensions.  There are some clandestine patches for glibc
   floating around, as well as a stanalone libaltivec library that
   implements such extensions.  */

#include "harness.h"
#include <string.h>

static void test()
{
  const char *x;
  char buf[256];

  vector float f32;
  vector signed char s8;
  vector unsigned short u16;
  vector signed short s16;
  vector signed int s32;

  f32 = ((vector float){1, 2.2, 333.333e-18, -4.5007544});

  x = "%vf: 1.000000 2.200000 0.000000 -4.500754";
  snprintf(buf, sizeof buf, "%%vf: %vf", f32);
  check(!strcmp(buf, x), x);

  x = "%ve: 1.000000e+00 2.200000e+00 3.333330e-16 -4.500754e+00";
  snprintf(buf, sizeof buf, "%%ve: %ve", f32);
  check(!strcmp(buf, x), x);

  x = "%vE: 1.000000E+00 2.200000E+00 3.333330E-16 -4.500754E+00";
  snprintf(buf, sizeof buf, "%%vE: %vE", f32);
  check(!strcmp(buf, x), x);

  x = "%vg: 1 2.2 3.33333e-16 -4.50075";
  snprintf(buf, sizeof buf, "%%vg: %vg", f32);
  check(!strcmp(buf, x), x);

  x = "%vG: 1 2.2 3.33333E-16 -4.50075";
  snprintf(buf, sizeof buf, "%%vG: %vG", f32);
  check(!strcmp(buf, x), x);

  x = "%7.3vf:   1.000   2.200   0.000  -4.501";
  snprintf(buf, sizeof buf, "%%7.3vf: %7.3vf", f32);
  check(!strcmp(buf, x), x);

  x = "%:7.3vf:   1.000:  2.200:  0.000: -4.501";
  snprintf(buf, sizeof buf, "%%:7.3vf: %:7.3vf", f32);
  check(!strcmp(buf, x), x);

  x = "%:7.3f: :7.3f";
  snprintf(buf, sizeof buf, "%%:7.3f: %:7.3f", f32);
  check(!strcmp(buf, x), x);


  s8 = vec_sub((vector signed char)vec_lvsl(1,(signed char *)0),
	       ((vector signed char){8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8}));
  x = "%vd: -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vd: %vd", s8);
  check(!strcmp(buf, x), x);

  x = "%vi: -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vi: %vi", s8);
  check(!strcmp(buf, x), x);

  x = "%vu: 249 250 251 252 253 254 255 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vu: %vu", s8);
  check(!strcmp(buf, x), x);

  x = "%vx: f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vx: %vx", s8);
  check(!strcmp(buf, x), x);

  x = "%vo: 371 372 373 374 375 376 377 0 1 2 3 4 5 6 7 10";
  snprintf(buf, sizeof buf, "%%vo: %vo", s8);
  check(!strcmp(buf, x), x);

  x = "%vp: 0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8";
  snprintf(buf, sizeof buf, "%%vp: %vp", s8);
  check(!strcmp(buf, x), x);

  x = "%vx: f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vx: %vx", s8);
  check(!strcmp(buf, x), x);

  x = "%vX: F9 FA FB FC FD FE FF 0 1 2 3 4 5 6 7 8";
  snprintf(buf, sizeof buf, "%%vX: %vX", s8);
  check(!strcmp(buf, x), x);


  x = "%hvd: -1542 -1028 -514 -256 258 772 1286 1800";
  snprintf(buf, sizeof buf, "%%hvd: %hvd", s8);
  check(!strcmp(buf, x), x);

  x = "%vhi: -1542 -1028 -514 -256 258 772 1286 1800";
  snprintf(buf, sizeof buf, "%%vhi: %vhi", s8);
  check(!strcmp(buf, x), x);

  x = "%hvu: 63994 64508 65022 65280 258 772 1286 1800";
  snprintf(buf, sizeof buf, "%%hvu: %hvu", s8);
  check(!strcmp(buf, x), x);

  x = "%vhx: f9fa fbfc fdfe ff00 102 304 506 708";
  snprintf(buf, sizeof buf, "%%vhx: %vhx", s8);
  check(!strcmp(buf, x), x);

  x = "%hvo: 174772 175774 176776 177400 402 1404 2406 3410";
  snprintf(buf, sizeof buf, "%%hvo: %hvo", s8);
  check(!strcmp(buf, x), x);

  x = "%vhp: 0xf9fa 0xfbfc 0xfdfe 0xff00 0x102 0x304 0x506 0x708";
  snprintf(buf, sizeof buf, "%%vhp: %vhp", s8);
  check(!strcmp(buf, x), x);

  x = "%hvx: f9fa fbfc fdfe ff00 102 304 506 708";
  snprintf(buf, sizeof buf, "%%hvx: %hvx", s8);
  check(!strcmp(buf, x), x);

  x = "%vhX: F9FA FBFC FDFE FF00 102 304 506 708";
  snprintf(buf, sizeof buf, "%%vhX: %vhX", s8);
  check(!strcmp(buf, x), x);

  x = "0x%_04vhx: 0xf9fa_fbfc_fdfe_ff00_0102_0304_0506_0708";
  snprintf(buf, sizeof buf, "0x%%_04vhx: 0x%_04vhx", s8);
  check(!strcmp(buf, x), x);


  x = "%lvd: -100992004 -33620224 16909060 84281096";
  snprintf(buf, sizeof buf, "%%lvd: %lvd", s8);
  check(!strcmp(buf, x), x);

  x = "%vli: -100992004 -33620224 16909060 84281096";
  snprintf(buf, sizeof buf, "%%vli: %vli", s8);
  check(!strcmp(buf, x), x);

  x = "%lvu: 4193975292 4261347072 16909060 84281096";
  snprintf(buf, sizeof buf, "%%lvu: %lvu", s8);
  check(!strcmp(buf, x), x);

  x = "%vlx: f9fafbfc fdfeff00 1020304 5060708";
  snprintf(buf, sizeof buf, "%%vlx: %vlx", s8);
  check(!strcmp(buf, x), x);

  x = "%lvo: 37176575774 37577577400 100401404 501403410";
  snprintf(buf, sizeof buf, "%%lvo: %lvo", s8);
  check(!strcmp(buf, x), x);

  x = "%vlp: 0xf9fafbfc 0xfdfeff00 0x1020304 0x5060708";
  snprintf(buf, sizeof buf, "%%vlp: %vlp", s8);
  check(!strcmp(buf, x), x);

  x = "%lvx: f9fafbfc fdfeff00 1020304 5060708";
  snprintf(buf, sizeof buf, "%%lvx: %lvx", s8);
  check(!strcmp(buf, x), x);

  x = "%vlX: F9FAFBFC FDFEFF00 1020304 5060708";
  snprintf(buf, sizeof buf, "%%vlX: %vlX", s8);
  check(!strcmp(buf, x), x);

  x = ">-154234 ;-1028   ;+0258   ;+1800   <";
  printf(">%- +;8.4vld<", ((vector signed int){-154234, -1028, 258, 1800}));
  check(!strcmp(buf, x), x);

  x = "%,2vx: f9,fa,fb,fc,fd,fe,ff, 0, 1, 2, 3, 4, 5, 6, 7, 8";
  snprintf(buf, sizeof buf, "%%,2vx: %,2vx", s8);
  check(!strcmp(buf, x), x);

  x = "%;4vhX: F9FA;FBFC;FDFE;FF00; 102; 304; 506; 708";
  snprintf(buf, sizeof buf, "%%;4vhX: %;4vhX", s8);
  check(!strcmp(buf, x), x);

  x = "%;8vlx: f9fafbfc;fdfeff00; 1020304; 5060708";
  snprintf(buf, sizeof buf, "%%;8vlx: %;8vlx", s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+8.5vhd: -01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  snprintf(buf, sizeof buf, "%%- ;+8.5vhd: %- ;+8.5vhd", s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+*.5vhd: -01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  snprintf(buf, sizeof buf, "%%- ;+*.5vhd: %- ;+*.5vhd", 8, s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+8.*vhd: -01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  snprintf(buf, sizeof buf, "%%- ;+8.*vhd: %- ;+8.*vhd", 5, s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+*.*vhd: -01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  snprintf(buf, sizeof buf, "%%- ;+*.*vhd: %- ;+*.*vhd", 8, 5, s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+7.4vhd: -1542  ;-1028  ;-0514  ;-0256  ;+0258  ;+0772  ;+1286  ;+1800  ";
  snprintf(buf, sizeof buf, "%%- ;+7.4vhd: %- ;+7.4vhd", s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+*.4vhd: -1542  ;-1028  ;-0514  ;-0256  ;+0258  ;+0772  ;+1286  ;+1800  ";
  snprintf(buf, sizeof buf, "%%- ;+*.4vhd: %- ;+*.4vhd", 7, s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+7.*vhd: -1542  ;-1028  ;-0514  ;-0256  ;+0258  ;+0772  ;+1286  ;+1800  ";
  snprintf(buf, sizeof buf, "%%- ;+7.*vhd: %- ;+7.*vhd", 4, s8);
  check(!strcmp(buf, x), x);

  x = "%- ;+*.*vhd: -1542  ;-1028  ;-0514  ;-0256  ;+0258  ;+0772  ;+1286  ;+1800  ";
  snprintf(buf, sizeof buf, "%%- ;+*.*vhd: %- ;+*.*vhd", 7, 4, s8);
  check(!strcmp(buf, x), x);


  s8 = vec_add(s8, ((vector signed char){'h','h','h','h','h','h','h','h',
					'h','h','h','h','h','h','h','h'}));
  x = "%vc: abcdefghijklmnop";
  snprintf(buf, sizeof buf, "%%vc: %vc", s8);
  check(!strcmp(buf, x), x);

  x = "abcdefghijklmnopqrstuvwxyz,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p";
  snprintf(buf, sizeof buf, "%vcqrstuvwxyz,%,vc", s8, s8);
  check(!strcmp(buf, x), x);

  x = "%#0,2vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70";
  snprintf(buf, sizeof buf, "%%#0,2vx: %#0,2vx", s8);
  check(!strcmp(buf, x), x);


  s8 = ((vector signed char){'H','e','l','l','o',' ','W','o','r','l','d',',',' ','.','.','.' });
  x = "s8  = Hello World, ...";
  snprintf(buf, sizeof buf, "s8  = %vc", s8);
  check(!strcmp(buf, x), x);

  x = "s8  = H,e,l,l,o, ,W,o,r,l,d,,, ,.,.,.";
  snprintf(buf, sizeof buf, "s8  = %,vc", s8);
  check(!strcmp(buf, x), x);


  s16 = ((vector signed short){-2,-1,0,1,2,3,4,5});
  x = "s16 = -2 -1 0 1 2 3 4 5";
  snprintf(buf, sizeof buf, "s16 = %vhd", s16);
  check(!strcmp(buf, x), x);


  u16 = ((vector unsigned short){65534,65535,0,1,2,3,4,5});
  x = "u16 = 65534,65535,0,1,2,3,4,5";
  snprintf(buf, sizeof buf, "u16 = %,vhu", u16);
  check(!strcmp(buf, x), x);


  s32 = ((vector signed int){1,2,3,99});
  x = "s32 =  1, 2, 3,99";
  snprintf(buf, sizeof buf, "s32 = %,2lvd", s32);
  check(!strcmp(buf, x), x);


  f32 = ((vector float){1.1, 2.2, 3.3, 4.39501});
  x = "f32 = 1.10 ,2.20 ,3.30 ,4.40 ";
  snprintf(buf, sizeof buf, "f32 = %-,5.2vf", f32);
  check(!strcmp(buf, x), x);


  /* <char-conv> only goes with <vector-size> 'v'.  */
  x = "u16 = vhc";
  snprintf(buf, sizeof buf, "u16 = %vhc", u16);
  check(!strcmp(buf, x), x);

  x = "s32 = vhc";
  snprintf(buf, sizeof buf, "s32 = %vhc", s32);
  check(!strcmp(buf, x), x);
}
