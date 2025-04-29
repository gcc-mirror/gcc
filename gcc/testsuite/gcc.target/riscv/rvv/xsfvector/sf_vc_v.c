/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvcp -mabi=lp64d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "sifive_vector.h"


/*
** test_sf_vc_v_vv_u8mf8:
** ...
** vsetivli\s+zero+,0+,e8+,mf8,ta,ma+
** sf\.vc\.v\.vv\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint8mf8_t test_sf_vc_v_vv_u8mf8(vuint8mf8_t vs2, vuint8mf8_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vv_u8mf8(1, vs2, rs1, vl);
}

/*
** test_sf_vc_v_vv_se_u8mf8:
** ...
** vsetivli\s+zero+,0+,e8+,mf8,ta,ma+
** sf\.vc\.v\.vv\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint8mf8_t test_sf_vc_v_vv_se_u8mf8(vuint8mf8_t vs2, vuint8mf8_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vv_se_u8mf8(1, vs2, rs1, vl);
}

/*
** test_sf_vc_vv_se_u16m1:
** ...
** vsetivli\s+zero+,0+,e16+,m1,ta,ma+
** sf\.vc\.vv\t[0-9]+,[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
void test_sf_vc_vv_se_u16m1(vuint16m1_t vs2, vuint16m1_t rs1, size_t vl) {
    __riscv_sf_vc_vv_se_u16m1(1, 3, vs2, rs1, vl);
}

/*
** test_sf_vc_v_vvv_u32mf2:
** ...
** vsetivli\s+zero+,0+,e32+,mf2,ta,ma+
** sf\.vc\.v\.vvv\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint32mf2_t test_sf_vc_v_vvv_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vvv_u32mf2(1, vd, vs2, rs1, vl);
}

/*
** test_sf_vc_v_vvv_se_u32mf2:
** ...
** vsetivli\s+zero+,0+,e32+,mf2,ta,ma+
** sf\.vc\.v\.vvv\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint32mf2_t test_sf_vc_v_vvv_se_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vvv_se_u32mf2(1, vd, vs2, rs1, vl);
}

/*
** test_sf_vc_vvv_se_u64m1:
** ...
** vsetivli\s+zero+,0+,e64+,m1,ta,ma+
** sf\.vc\.vvv\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
void test_sf_vc_vvv_se_u64m1(vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t rs1, size_t vl) {
    __riscv_sf_vc_vvv_se_u64m1(1, vd, vs2, rs1, vl);
}


/*
** test_sf_vc_v_vvw_u8m1:
** ...
** vsetivli\s+zero+,0+,e8+,m1,ta,ma+
** sf\.vc\.v\.vvw\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint16m2_t test_sf_vc_v_vvw_u8m1(vuint16m2_t vd, vuint8m1_t vs2, vuint8m1_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vvw_u8m1(1, vd, vs2, rs1, vl);
}

/*
** test_sf_vc_v_vvw_se_u8m1:
** ...
** vsetivli\s+zero+,0+,e8+,m1,ta,ma+
** sf\.vc\.v\.vvw\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
vuint16m2_t test_sf_vc_v_vvw_se_u8m1(vuint16m2_t vd, vuint8m1_t vs2, vuint8m1_t rs1, size_t vl) {
    return __riscv_sf_vc_v_vvw_se_u8m1(1, vd, vs2, rs1, vl);
}

/*
** test_sf_vc_vvw_se_u16mf2:
** ...
** vsetivli\s+zero+,0+,e16+,mf2,ta,ma+
** sf\.vc\.vvw\t[0-9]+,v[0-9]+,v[0-9]+,v[0-9]+
** ...
*/
void test_sf_vc_vvw_se_u16mf2(vuint32m1_t vd, vuint16mf2_t vs2, vuint16mf2_t rs1, size_t vl) {
    __riscv_sf_vc_vvw_se_u16mf2(1, vd, vs2, rs1, vl);
}

