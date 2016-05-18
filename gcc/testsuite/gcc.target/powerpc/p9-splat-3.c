/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */

#include <altivec.h>

typedef vector signed char	v16qi_t;
typedef vector short		v8hi_t;
typedef vector int		v4si_t;
typedef vector long long	v2di_t;

void v16qi_0a  (v16qi_t *p) { *p = (v16qi_t) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }; }
void v8hi_0a   (v8hi_t  *p) { *p = (v8hi_t)  { 0, 0, 0, 0, 0, 0, 0, 0 }; }
void v4si_0a   (v4si_t  *p) { *p = (v4si_t)  { 0, 0, 0, 0 }; }
void v2di_0a   (v2di_t  *p) { *p = (v2di_t)  { 0, 0 }; }

void v16qi_0b  (v16qi_t *p) { *p = (v16qi_t) vec_splats ((signed char)0); }
void v8hi_0b   (v8hi_t  *p) { *p = (v8hi_t)  vec_splats ((short)0); }
void v4si_0b   (v4si_t  *p) { *p = (v4si_t)  vec_splats ((int)0); }
void v2di_0b   (v2di_t  *p) { *p = (v2di_t)  vec_splats ((long long)0); }

void v16qi_m1a (v16qi_t *p) { *p = (v16qi_t) { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }; }
void v8hi_m1a  (v8hi_t  *p) { *p = (v8hi_t)  { -1, -1, -1, -1, -1, -1, -1, -1 }; }
void v4si_m1a  (v4si_t  *p) { *p = (v4si_t)  { -1, -1, -1, -1 }; }
void v2di_m1a  (v2di_t  *p) { *p = (v2di_t)  { -1, -1 }; }

void v16qi_m1b (v16qi_t *p) { *p = (v16qi_t) vec_splats ((signed char)-1); }
void v8hi_m1b  (v8hi_t  *p) { *p = (v8hi_t)  vec_splats ((short)-1); }
void v4si_m1b  (v4si_t  *p) { *p = (v4si_t)  vec_splats ((int)-1); }
void v2di_m1b  (v2di_t  *p) { *p = (v2di_t)  vec_splats ((long long)-1); }

void v16qi_5a  (v16qi_t *p) { *p = (v16qi_t) { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 }; }
void v8hi_5a   (v8hi_t  *p) { *p = (v8hi_t)  { 5, 5, 5, 5, 5, 5, 5, 5 }; }
void v4si_5a   (v4si_t  *p) { *p = (v4si_t)  { 5, 5, 5, 5 }; }
void v2di_5a   (v2di_t  *p) { *p = (v2di_t)  { 5, 5 }; }

void v16qi_5b  (v16qi_t *p) { *p = (v16qi_t) vec_splats ((signed char)5); }
void v8hi_5b   (v8hi_t  *p) { *p = (v8hi_t)  vec_splats ((short)5); }
void v4si_5b   (v4si_t  *p) { *p = (v4si_t)  vec_splats ((int)5); }
void v2di_5b   (v2di_t  *p) { *p = (v2di_t)  vec_splats ((long long)5); }

void v16qi_33a (v16qi_t *p) { *p = (v16qi_t) { 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33 }; }
void v8hi_33a  (v8hi_t  *p) { *p = (v8hi_t)  { 33, 33, 33, 33, 33, 33, 33, 33 }; }
void v4si_33a  (v4si_t  *p) { *p = (v4si_t)  { 33, 33, 33, 33 }; }
void v2di_33a  (v2di_t  *p) { *p = (v2di_t)  { 33, 33 }; }

void v16qi_33b (v16qi_t *p) { *p = (v16qi_t) vec_splats ((signed char)33); }
void v8hi_33b  (v8hi_t  *p) { *p = (v8hi_t)  vec_splats ((short)33); }
void v4si_33b  (v4si_t  *p) { *p = (v4si_t)  vec_splats ((int)33); }
void v2di_33b  (v2di_t  *p) { *p = (v2di_t)  vec_splats ((long long)33); }

/* { dg-final { scan-assembler     "xxspltib"     } } */
/* { dg-final { scan-assembler     "vextsb2d"     } } */
/* { dg-final { scan-assembler     "vextsb2w"     } } */
/* { dg-final { scan-assembler     "vupk\[hl\]sb" } } */
/* { dg-final { scan-assembler-not "lxvd2x"       } } */
/* { dg-final { scan-assembler-not "lxvw4x"       } } */
/* { dg-final { scan-assembler-not "lxv "         } } */
/* { dg-final { scan-assembler-not "lxvx"         } } */
/* { dg-final { scan-assembler-not "lvx"          } } */
