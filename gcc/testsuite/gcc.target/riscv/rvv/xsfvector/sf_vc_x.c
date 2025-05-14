/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvcp -mabi=lp64d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sifive_vector.h"

/*
** test_sf_vc_v_x_u32m1:
** ...
** vsetivli\s+zero+,0+,e32+,m1,ta,ma+
** sf\.vc\.v\.x\t[0-9]+,[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint32m1_t test_sf_vc_v_x_u32m1(uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_x_u32m1(1, 2, xs1, vl);
}

/*
** test_sf_vc_v_x_se_u32m1:
** ...
** vsetivli\s+zero+,0+,e32+,m1,ta,ma+
** sf\.vc\.v\.x\t[0-9]+,[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint32m1_t test_sf_vc_v_x_se_u32m1(uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_x_se_u32m1(1, 2, xs1, vl);
}

/*
** test_sf_vc_x_se_u16m8:
** ...
** vsetivli\s+zero+,0+,e16+,m8,ta,ma+
** sf\.vc\.x\t[0-9]+,[0-9]+,[0-9]+,a[0-9]+
** ...
*/
void test_sf_vc_x_se_u16m8(uint16_t xs1, size_t vl) {
    __riscv_sf_vc_x_se_u16m8(1, 2, 3, xs1, vl);
}

/*
** test_sf_vc_v_xv_u32m2:
** ...
** vsetivli\s+zero+,0+,e32+,m2,ta,ma+
** sf\.vc\.v\.xv\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint32m2_t test_sf_vc_v_xv_u32m2(vuint32m2_t vs2, uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xv_u32m2(1, vs2, xs1, vl);
}

/*
** test_sf_vc_v_xv_se_u32m2:
** ...
** vsetivli\s+zero+,0+,e32+,m2,ta,ma+
** sf\.vc\.v\.xv\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint32m2_t test_sf_vc_v_xv_se_u32m2(vuint32m2_t vs2, uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xv_se_u32m2(1, vs2, xs1, vl);
}

/*
** test_sf_vc_xv_se_u16m4:
** ...
** vsetivli\s+zero+,0+,e16+,m4,ta,ma+
** sf\.vc\.xv\t[0-9]+,[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
void test_sf_vc_xv_se_u16m4(vuint16m4_t vs2, uint16_t xs1, size_t vl) {
    __riscv_sf_vc_xv_se_u16m4(1, 3, vs2, xs1, vl);
}

/*
** test_sf_vc_v_xvv_u16m1:
** ...
** vsetivli\s+zero+,0+,e16+,m1,ta,ma+
** sf\.vc\.v\.xvv\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint16m1_t test_sf_vc_v_xvv_u16m1(vuint16m1_t vd, vuint16m1_t vs2, uint16_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xvv_u16m1(1, vd, vs2, xs1, vl);
}

/*
** test_sf_vc_v_xvv_se_u16m1:
** ...
** vsetivli\s+zero+,0+,e16+,m1,ta,ma+
** sf\.vc\.v\.xvv\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint16m1_t test_sf_vc_v_xvv_se_u16m1(vuint16m1_t vd, vuint16m1_t vs2, uint16_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xvv_se_u16m1(1, vd, vs2, xs1, vl);
}

/*
** test_sf_vc_xvv_se_u32m2:
** ...
** vsetivli\s+zero+,0+,e32+,m2,ta,ma+
** sf\.vc\.xvv\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
void test_sf_vc_xvv_se_u32m2(vuint32m2_t vd, vuint32m2_t vs2, uint32_t xs1, size_t vl) {
    __riscv_sf_vc_xvv_se_u32m2(1, vd, vs2, xs1, vl);
}

/*
** test_sf_vc_v_xvw_u32m1:
** ...
** vsetivli\s+zero+,0+,e32+,m1,ta,ma+
** sf\.vc\.v\.xvw\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint64m2_t test_sf_vc_v_xvw_u32m1(vuint64m2_t vd, vuint32m1_t vs2, uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xvw_u32m1(1, vd, vs2, xs1, vl);
}

/*
** test_sf_vc_v_xvw_se_u32m1:
** ...
** vsetivli\s+zero+,0+,e32+,m1,ta,ma+
** sf\.vc\.v\.xvw\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
vuint64m2_t test_sf_vc_v_xvw_se_u32m1(vuint64m2_t vd, vuint32m1_t vs2, uint32_t xs1, size_t vl) {
    return __riscv_sf_vc_v_xvw_se_u32m1(1, vd, vs2, xs1, vl);
}

/*
** test_sf_vc_xvw_se_u32m1:
** ...
** vsetivli\s+zero+,0+,e32+,m1,ta,ma+
** sf\.vc\.xvw\t[0-9]+,v[0-9]+,v[0-9]+,a[0-9]+
** ...
*/
void test_sf_vc_xvw_se_u32m1(vuint64m2_t vd, vuint32m1_t vs2, uint32_t xs1, size_t vl) {
    __riscv_sf_vc_xvw_se_u32m1(1, vd, vs2, xs1, vl);
}

