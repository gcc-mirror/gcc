/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* PR90763: PowerPC vec_xl_len should take const.
*/

#include <altivec.h>

vector unsigned char vec_load_uc(unsigned char *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned char vec_load_const_uc(const unsigned char *p, int num) {
    return vec_xl_len(p, num);
}
vector signed char vec_load_sc(signed char *p, int num) {
    return vec_xl_len(p, num);
}
vector signed char vec_load_const_sc(const signed char *p, int num) {
    return vec_xl_len(p, num);
}

vector signed short vec_load_ss(signed short *p, int num) {
    return vec_xl_len(p, num);
}
vector signed short vec_load_const_ss(const signed short *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned short vec_load_us(unsigned short *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned short vec_load_const_us(const unsigned short *p, int num) {
    return vec_xl_len(p, num);
}

vector signed int vec_load_si(signed int *p, int num) {
    return vec_xl_len(p, num);
}
vector signed int vec_load_const_si(const signed int *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned int vec_load_ui(unsigned int *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned int vec_load_const_ui(const unsigned int *p, int num) {
    return vec_xl_len(p, num);
}

vector signed long long vec_load_sll(signed long long *p, int num) {
    return vec_xl_len(p, num);
}
vector signed long long vec_load_const_sll(const signed long long *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned long long vec_load_ull(unsigned long long *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned long long vec_load_const_ull(const unsigned long long *p, int num) {
    return vec_xl_len(p, num);
}

vector signed __int128 vec_load_si128(signed __int128 *p, int num) {
    return vec_xl_len(p, num);
}
vector signed __int128 vec_load_const_si128(const signed __int128 *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned __int128 vec_load_ui128(unsigned __int128 *p, int num) {
    return vec_xl_len(p, num);
}
vector unsigned __int128 vec_load_const_ui128(const unsigned __int128 *p, int num) {
    return vec_xl_len(p, num);
}

vector float vec_load_f(float *p, int num) {
    return vec_xl_len(p, num);
}
vector float vec_load_const_f(const float *p, int num) {
    return vec_xl_len(p, num);
}

vector double vec_load_d(double *p, int num) {
    return vec_xl_len(p, num);
}
vector double vec_load_const_d(const double *p, int num) {
    return vec_xl_len(p, num);
}

