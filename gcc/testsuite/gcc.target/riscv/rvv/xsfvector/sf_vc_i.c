/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvcp -mabi=lp64d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sifive_vector.h"


/*
** test_sf_vc_v_i_u16m4:
** ...
** vsetivli\s+zero+,0+,e16+,m4,ta,ma+
** sf\.vc\.v\.i\t[0-9]+,[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint16m4_t test_sf_vc_v_i_u16m4(size_t vl) {
    return __riscv_sf_vc_v_i_u16m4(1, 2, 4, vl);
}

/*
** test_sf_vc_v_i_se_u16m4:
** ...
** vsetivli\s+zero+,0+,e16+,m4,ta,ma+
** sf\.vc\.v\.i\t[0-9]+,[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint16m4_t test_sf_vc_v_i_se_u16m4(size_t vl) {
    return __riscv_sf_vc_v_i_se_u16m4(1, 2, 4, vl);
}

/*
** test_sf_vc_i_se_u16mf4:
** ...
** vsetivli\s+zero+,0+,e16+,mf4,ta,ma+
** sf\.vc\.i\t[0-9]+,[0-9]+,[0-9]+,[0-9]+
** ...
*/
void test_sf_vc_i_se_u16mf4(size_t vl) {
    __riscv_sf_vc_i_se_u16mf4(1, 2, 3, 4, vl);
}

/*
** test_sf_vc_v_iv_u32m2:
** ...
** vsetivli\s+zero+,0+,e32+,m2,ta,ma+
** sf\.vc\.v\.iv\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint32m2_t test_sf_vc_v_iv_u32m2(vuint32m2_t vs2, size_t vl) {
    return __riscv_sf_vc_v_iv_u32m2(1, vs2, 4, vl);
}

/*
** test_sf_vc_v_iv_se_u32m2:
** ...
** vsetivli\s+zero+,0+,e32+,m2,ta,ma+
** sf\.vc\.v\.iv\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint32m2_t test_sf_vc_v_iv_se_u32m2(vuint32m2_t vs2, size_t vl) {
    return __riscv_sf_vc_v_iv_se_u32m2(1, vs2, 4, vl);
}

/*
** test_sf_vc_iv_se_u16m2:
** ...
** vsetivli\s+zero+,0+,e16+,m2,ta,ma+
** sf\.vc\.iv\t[0-9]+,[0-9]+,v[0-9]+,[0-9]+
** ...
*/
void test_sf_vc_iv_se_u16m2(vuint16m2_t vs2, size_t vl) {
    __riscv_sf_vc_iv_se_u16m2(1, 3, vs2, 4, vl);
}

/*
** test_sf_vc_v_ivv_u8m8:
** ...
** vsetivli\s+zero+,0+,e8+,m8,ta,ma+
** sf\.vc\.v\.ivv\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint8m8_t test_sf_vc_v_ivv_u8m8(vuint8m8_t vd, vuint8m8_t vs2, size_t vl) {
    return __riscv_sf_vc_v_ivv_u8m8(1, vd, vs2, 4, vl);
}

/*
** test_sf_vc_v_ivv_se_u8m8:
** ...
** vsetivli\s+zero+,0+,e8+,m8,ta,ma+
** sf\.vc\.v\.ivv\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint8m8_t test_sf_vc_v_ivv_se_u8m8(vuint8m8_t vd, vuint8m8_t vs2, size_t vl) {
    return __riscv_sf_vc_v_ivv_se_u8m8(1, vd, vs2, 4, vl);
}

/*
** test_sf_vc_ivv_se_u64m1:
** ...
** vsetivli\s+zero+,0+,e64+,m1,ta,ma+
** sf\.vc\.ivv\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
void test_sf_vc_ivv_se_u64m1(vuint64m1_t vd, vuint64m1_t vs2, size_t vl) {
    __riscv_sf_vc_ivv_se_u64m1(1, vd, vs2, 4, vl);
}

/*
** test_sf_vc_v_ivw_u8mf4:
** ...
** vsetivli\s+zero+,0+,e8+,mf4,ta,ma+
** sf\.vc\.v\.ivw\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint16mf2_t test_sf_vc_v_ivw_u8mf4(vuint16mf2_t vd, vuint8mf4_t vs2, size_t vl) {
    return __riscv_sf_vc_v_ivw_u8mf4(1, vd, vs2, 4, vl);
}

/*
** test_sf_vc_v_ivw_se_u8mf4:
** ...
** vsetivli\s+zero+,0+,e8+,mf4,ta,ma+
** sf\.vc\.v\.ivw\t[0-9]+,v[0-9]+,v[0-9]+,[0-9]+
** ...
*/
vuint16mf2_t test_sf_vc_v_ivw_se_u8mf4(vuint16mf2_t vd, vuint8mf4_t vs2, size_t vl) {
    return __riscv_sf_vc_v_ivw_se_u8mf4(1, vd, vs2, 4, vl);
}

void test_sf_vc_ivw_se_u32m4(vuint64m8_t vd, vuint32m4_t vs2, size_t vl) {
    __riscv_sf_vc_ivw_se_u32m4(1, vd, vs2, 4, vl);
}

