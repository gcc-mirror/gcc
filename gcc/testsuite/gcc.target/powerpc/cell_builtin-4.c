/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -maltivec -mdejagnu-cpu=cell" } */
/* { dg-final { scan-assembler-times "lvrxl" 19 } } */

#include <altivec.h>

typedef __vector signed char vsc;
typedef __vector signed short vss;
typedef __vector signed int vsi;
typedef __vector unsigned char vuc;
typedef __vector unsigned short vus;
typedef __vector unsigned int vui;
typedef __vector bool char vbc;
typedef __vector bool short vbs;
typedef __vector bool int vbi;
typedef __vector float vsf;
typedef __vector pixel vp;
typedef signed char sc;
typedef signed short ss;
typedef signed int si;
typedef signed long sl;
typedef unsigned char uc;
typedef unsigned short us;
typedef unsigned int ui;
typedef unsigned long ul;
typedef float sf;

vsc  lc4(long a, void *p)           { return __builtin_altivec_lvrxl (a,p); }
vsf  lrxl01(long a, vsf *p)         { return __builtin_vec_lvrxl (a,p); }
vsf  lrxl02(long a, sf *p)          { return __builtin_vec_lvrxl (a,p); }
vbi  lrxl03(long a, vbi *p)         { return __builtin_vec_lvrxl (a,p); }
vsi  lrxl04(long a, vsi *p)         { return __builtin_vec_lvrxl (a,p); }
vsi  lrxl05(long a, si *p)          { return __builtin_vec_lvrxl (a,p); }
vui  lrxl06(long a, vui *p)         { return __builtin_vec_lvrxl (a,p); }
vui  lrxl07(long a, ui *p)          { return __builtin_vec_lvrxl (a,p); }
vbs  lrxl08(long a, vbs *p)         { return __builtin_vec_lvrxl (a,p); }
vp   lrxl09(long a, vp *p)          { return __builtin_vec_lvrxl (a,p); }
vss  lrxl10(long a, vss *p)         { return __builtin_vec_lvrxl (a,p); }
vss  lrxl11(long a, ss *p)          { return __builtin_vec_lvrxl (a,p); }
vus  lrxl12(long a, vus *p)         { return __builtin_vec_lvrxl (a,p); }
vus  lrxl13(long a, us *p)          { return __builtin_vec_lvrxl (a,p); }
vbc  lrxl14(long a, vbc *p)         { return __builtin_vec_lvrxl (a,p); }
vsc  lrxl15(long a, vsc *p)         { return __builtin_vec_lvrxl (a,p); }
vsc  lrxl16(long a, sc *p)          { return __builtin_vec_lvrxl (a,p); }
vuc  lrxl17(long a, vuc *p)         { return __builtin_vec_lvrxl (a,p); }
vuc  lrxl18(long a, uc *p)          { return __builtin_vec_lvrxl (a,p); }
