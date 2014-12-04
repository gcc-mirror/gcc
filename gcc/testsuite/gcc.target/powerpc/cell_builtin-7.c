/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=cell" } } */
/* { dg-options "-O2 -maltivec -mcpu=cell" } */
/* { dg-final { scan-assembler-times "stvrx" 19 } } */

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

void sc3(vsc v, long a, void *p)    { __builtin_altivec_stvrx (v,a,p); }
void srx01(vsf v, long a, vsf *p)   { __builtin_vec_stvrx (v,a,p); }
void srx02(vsf v, long a, sf *p)    { __builtin_vec_stvrx (v,a,p); }
void srx03(vbi v, long a, vbi *p)   { __builtin_vec_stvrx (v,a,p); }
void srx04(vsi v, long a, vsi *p)   { __builtin_vec_stvrx (v,a,p); }
void srx05(vsi v, long a, si *p)    { __builtin_vec_stvrx (v,a,p); }
void srx06(vui v, long a, vui *p)   { __builtin_vec_stvrx (v,a,p); }
void srx07(vui v, long a, ui *p)    { __builtin_vec_stvrx (v,a,p); }
void srx08(vbs v, long a, vbs *p)   { __builtin_vec_stvrx (v,a,p); }
void srx09(vp v, long a, vp *p)     { __builtin_vec_stvrx (v,a,p); }
void srx10(vss v, long a, vss *p)   { __builtin_vec_stvrx (v,a,p); }
void srx11(vss v, long a, ss *p)    { __builtin_vec_stvrx (v,a,p); }
void srx12(vus v, long a, vus *p)   { __builtin_vec_stvrx (v,a,p); }
void srx13(vus v, long a, us *p)    { __builtin_vec_stvrx (v,a,p); }
void srx14(vbc v, long a, vbc *p)   { __builtin_vec_stvrx (v,a,p); }
void srx15(vsc v, long a, vsc *p)   { __builtin_vec_stvrx (v,a,p); }
void srx16(vsc v, long a, sc *p)    { __builtin_vec_stvrx (v,a,p); }
void srx17(vuc v, long a, vuc *p)   { __builtin_vec_stvrx (v,a,p); }
void srx18(vuc v, long a, uc *p)    { __builtin_vec_stvrx (v,a,p); }
