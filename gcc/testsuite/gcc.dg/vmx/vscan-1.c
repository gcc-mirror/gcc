/* These tests require an AltiVec aware C library with vector
   extensions for scanf (%v).  Currently newlib handles AltiVec
   vector extensions.  There are some clandestine patches for glibc
   floating around, as well as a stanalone libaltivec library that
   implements such extensions.  */

#include "harness.h"
#include <string.h>

void test()
{
  int n, i;
  const char *p;
  const char *x;
  char buf[256];

  char ch;
  vector float f32;
  vector signed char s8, s8a;
  vector unsigned short u16;
  vector signed short s16;
  vector signed int s32;

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "%vf: 1.000000 2.200000 0.000000 -4.500754;";
  x = "1:42:42:1.000000 2.200000 0.000000 -4.500754";
  n = sscanf(p, "%%vf: %vf;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vf", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "%ve: 1.000000e+00 2.200000e+00 3.333330e-16 -4.500754e+00;";
  x = "1:58:58:1.000000e+00 2.200000e+00 3.333330e-16 -4.500754e+00";
  n = sscanf(p, "%%ve: %ve;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%ve", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "%vg: 1 2.2 3.33333e-16 -4.50075;";
  x = "1:32:32:1 2.2 3.33333e-16 -4.50075";
  n = sscanf(p, "%%vg: %vg;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vg", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "%vG: 1 2.2 3.33333E-16 -4.50075;";
  x = "1:32:32:1 2.2 3.33333E-16 -4.50075";
  n = sscanf(p, "%%vG: %vG;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vG", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "%:7.3vf:   1.000:  2.200:  0.000: -4.501;";
  x = "1:41:41:  1.000:  2.200:  0.000: -4.501";
  n = sscanf(p, "%%:7.3vf:%:7vf;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%:7.3vf", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vd: -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8;";
  x = "1:44:44:-7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vd:%vd;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vd", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vi: -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8;";
  x = "1:44:44:-7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vi:%vi;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vi", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vu: 249 250 251 252 253 254 255 0 1 2 3 4 5 6 7 8;";
  x = "1:51:51:249 250 251 252 253 254 255 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vu:%vu;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vu", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vx: f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8;";
  x = "1:44:44:f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vx:%vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vo: 371 372 373 374 375 376 377 0 1 2 3 4 5 6 7 10;";
  x = "1:52:52:371 372 373 374 375 376 377 0 1 2 3 4 5 6 7 10";
  n = sscanf(p, "%%vo:%vo;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vo", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vp: 0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8;";
  x = "1:76:76:0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8";
  n = sscanf(p, "%%vp:%vp;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vp", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vx: f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8;";
  x = "1:44:44:f9 fa fb fc fd fe ff 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vx:%vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vX: F9 FA FB FC FD FE FF 0 1 2 3 4 5 6 7 8;";
  x = "1:44:44:F9 FA FB FC FD FE FF 0 1 2 3 4 5 6 7 8";
  n = sscanf(p, "%%vX:%vX;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vX", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%hvd: -1542 -1028 -514 -256 258 772 1286 1800;";
  x = "1:46:46:-1542 -1028 -514 -256 258 772 1286 1800";
  n = sscanf(p, "%%hvd:%hvd;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%hvd", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vhi: -1542 -1028 -514 -256 258 772 1286 1800;";
  x = "1:46:46:-1542 -1028 -514 -256 258 772 1286 1800";
  n = sscanf(p, "%%vhi:%vhi;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vhi", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%hvu: 63994 64508 65022 65280 258 772 1286 1800;";
  x = "1:48:48:63994 64508 65022 65280 258 772 1286 1800";
  n = sscanf(p, "%%hvu:%hvu;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%hvu", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vhx: f9fa fbfc fdfe ff00 102 304 506 708;";
  x = "1:42:42:f9fa fbfc fdfe ff00 102 304 506 708";
  n = sscanf(p, "%%vhx:%vhx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vhx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "0x%_vhx: 0xf9fa_fbfc_fdfe_ff00_0102_0304_0506_0708;";
  x = "1:51:51:0xf9fa_fbfc_fdfe_ff00_0102_0304_0506_0708";
  n = sscanf(p, "0x%%_vhx:%_vhx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:0x%_04vhx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%hvo: 174772 175774 176776 177400 402 1404 2406 3410;";
  x = "1:53:53:174772 175774 176776 177400 402 1404 2406 3410";
  n = sscanf(p, "%%hvo:%hvo;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%hvo", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vhp: 0xf9fa 0xfbfc 0xfdfe 0xff00 0x102 0x304 0x506 0x708;";
  x = "1:58:58:0xf9fa 0xfbfc 0xfdfe 0xff00 0x102 0x304 0x506 0x708";
  n = sscanf(p, "%%vhp:%vhp;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vhp", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%hvx: f9fa fbfc fdfe ff00 102 304 506 708;";
  x = "1:42:42:f9fa fbfc fdfe ff00 102 304 506 708";
  n = sscanf(p, "%%hvx:%hvx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%hvx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vhX: F9FA FBFC FDFE FF00 102 304 506 708;";
  x = "1:42:42:F9FA FBFC FDFE FF00 102 304 506 708";
  n = sscanf(p, "%%vhX:%vhX;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vhX", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%lvd: -100992004 -33620224 16909060 84281096;";
  x = "1:45:45:-100992004 -33620224 16909060 84281096";
  n = sscanf(p, "%%lvd:%lvd;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%lvd", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vli: -100992004 -33620224 16909060 84281096;";
  x = "1:45:45:-100992004 -33620224 16909060 84281096";
  n = sscanf(p, "%%vli:%vli;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vli", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%lvu: 4193975292 4261347072 16909060 84281096;";
  x = "1:46:46:4193975292 4261347072 16909060 84281096";
  n = sscanf(p, "%%lvu:%lvu;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%lvu", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vlx: f9fafbfc fdfeff00 1020304 5060708;";
  x = "1:40:40:f9fafbfc fdfeff00 1020304 5060708";
  n = sscanf(p, "%%vlx:%vlx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vlx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%lvo: 37176575774 37577577400 100401404 501403410;";
  x = "1:50:50:37176575774 37577577400 100401404 501403410";
  n = sscanf(p, "%%lvo:%lvo;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%lvo", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vlp: 0xf9fafbfc 0xfdfeff00 0x1020304 0x5060708;";
  x = "1:48:48:0xf9fafbfc 0xfdfeff00 0x1020304 0x5060708";
  n = sscanf(p, "%%vlp:%vlp;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vlp", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%lvx: f9fafbfc fdfeff00 1020304 5060708;";
  x = "1:40:40:f9fafbfc fdfeff00 1020304 5060708";
  n = sscanf(p, "%%lvx:%lvx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%lvx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vlX: F9FAFBFC FDFEFF00 1020304 5060708;";
  x = "1:40:40:F9FAFBFC FDFEFF00 1020304 5060708";
  n = sscanf(p, "%%vlX:%vlX;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vlX", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,2vx: f9,fa,fb,fc,fd,fe,ff, 0, 1, 2, 3, 4, 5, 6, 7, 8;";
  x = "1:55:55:f9,fa,fb,fc,fd,fe,ff, 0, 1, 2, 3, 4, 5, 6, 7, 8";
  n = sscanf(p, "%%,2vx:%,2vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,2vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%;4vhX: F9FA;FBFC;FDFE;FF00; 102; 304; 506; 708;";
  x = "1:48:48:F9FA;FBFC;FDFE;FF00; 102; 304; 506; 708";
  n = sscanf(p, "%%;4vhX:%;4vhX;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%;4vhX", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%;8vlx: f9fafbfc;fdfeff00; 1020304; 5060708;";
  x = "1:44:44:f9fafbfc;fdfeff00; 1020304; 5060708";
  n = sscanf(p, "%%;8vlx:%;8vlx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%;8vlx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%;8vhd: -01542;  -01028;  -00514;  -00256;  +00258;  +00772;  +01286;  +01800;";
  x = "1:78:78:-01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  n = sscanf(p, "%%;8vhd:%;8vhd;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%- ;+8.5vhd", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%;8vhd: -01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ;";
  x = "1:80:80:-01542  ;-01028  ;-00514  ;-00256  ;+00258  ;+00772  ;+01286  ;+01800  ";
  n = sscanf(p, "%%;8vhd:%;8vhd ;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%- ;+8.5vhd", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vc: ab defghijklmnop;";
  x = "1:22:22:ab defghijklmnop";
  n = sscanf(p, "%%vc: %vc;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vc", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%vcqrstuvwxyz,%,vc: ab defghijklmnopqrstuvwxyz,a,b, ,d,e,f,g,h,i,j,k,l,m,n,o,p;";
  x = "2:79:79:ab defghijklmnopqrstuvwxyz,a,b, ,d,e,f,g,h,i,j,k,l,m,n,o,p";
  n = sscanf(p, "%%vcqrstuvwxyz,%%,vc: %vcqrstuvwxyz,%,vc;%n", &s8, &s8a, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vcqrstuvwxyz,%,vc", n, i, strlen(p), s8, s8a);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61, 0x62 ,0x63 , 0x64,0x65  ,0x66,  0x67  ,  0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70;";
  x = "1:98:98:0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* Input conflict after 0x70.  two assignments; %n not processed.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70#";
  x = "1:-1:86:0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* Input conflict after 0x67.  one assignment; %n not processed.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67#0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70;";
  x = "1:-1:86:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx%c%,vx;%n", &ch, &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* Input conflict after 0x7.  two assignments; %n not processed.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x7_0;";
  x = "2:-1:87:0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x7";
  n = sscanf(p, "%%,vx%c%,vx;%n", &ch, &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* EOF reached before the first assignment.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f";
  x = "-1:-1:80:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* EOF reached before the first assignment.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f ";
  x = "-1:-1:81:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* EOF reached before the first assignment.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f ,";
  x = "-1:-1:82:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* EOF reached before the first assignment.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f, ";
  x = "-1:-1:82:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* Input conflict after 0x7.  one assignment; %n not processed.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x7_0;";
  x = "1:-1:87:0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x7";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  /* Input conflict after 0x61.  zero assignments; %n not processed.  */
  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "%,vx: 0x61 0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,0x70;";
  x = "0:-1:86:0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0";
  n = sscanf(p, "%%,vx:%,vx;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%#,vx", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "Hello World, ...;";
  x = "1:17:17:Hello World, ...";
  n = sscanf(p, "%vc;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vc", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s8 = ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); i = -1;
  p = "H,e,l,l,o, ,W,o,r,l,d,,, ,.,.,.;";
  x = "1:32:32:H,e,l,l,o, ,W,o,r,l,d,,, ,.,.,.";
  n = sscanf(p, "%,vc;%n", &s8, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,vc", n, i, strlen(p), s8);
  check(!strcmp(x, buf), p);

  s16 = ((vector signed short){0,0,0,0,0,0,0,0}); i = -1;
  p = "-2 -1 0 1 2 3 4 5;";
  x = "1:18:18:-2 -1 0 1 2 3 4 5";
  n = sscanf(p, "%vhd;%n", &s16, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%vhd", n, i, strlen(p), s16);
  check(!strcmp(x, buf), p);

  u16 = ((vector unsigned short){0,0,0,0,0,0,0,0}); i = -1;
  p = "65534,65535,0,1,2,3,4,5;";
  x = "1:24:24:65534,65535,0,1,2,3,4,5";
  n = sscanf(p, "%,vhu;%n", &u16, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,vhu", n, i, strlen(p), u16);
  check(!strcmp(x, buf), p);

  s32 = ((vector signed int){0,0,0,0}); i = -1;
  p = " 1, 2, 3,99;";
  x = "1:12:12: 1, 2, 3,99";
  n = sscanf(p, "%,2lvd;%n", &s32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,2lvd", n, i, strlen(p), s32);
  check(!strcmp(x, buf), p);

  f32 = ((vector float){0,0,0,0}); i = -1;
  p = "1.10 ,2.20 ,3.30 ,4.40 ;";
  x = "1:24:24:1.10 ,2.20 ,3.30 ,4.40 ";
  n = sscanf(p, "%,5vf ;%n", &f32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%-,5.2vf", n, i, strlen(p), f32);
  check(!strcmp(x, buf), p);

  /* <char-conv> only goes with <vector-size> 'v'.  */
  u16 = ((vector unsigned short){0,0,0,0,0,0,0,0}); i = -1;
  p = "abcdefgh;";
  x = "0:-1:9:0,0,0,0,0,0,0,0";
  n = sscanf(p, "%vhc;%n", &u16, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,vhu", n, i, strlen(p), u16);
  check(!strcmp(x, buf), p);

  s32 = ((vector signed int){0,0,0,0}); i = -1;
  p = "abcd;";
  x = "0:-1:5:0,0,0,0";
  n = sscanf(p, "%lvc;%n", &s32, &i);
  snprintf(buf, sizeof buf, "%d:%d:%d:%,vlu", n, i, strlen(p), s32);
  check(!strcmp(x, buf), p);
}
