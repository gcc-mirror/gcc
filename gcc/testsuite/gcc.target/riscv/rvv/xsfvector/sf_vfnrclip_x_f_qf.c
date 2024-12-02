/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_xsfvfnrclipxfqf -mabi=lp64d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_vint8mf8_t(vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_vint8mf4_t(vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_vint8mf2_t(vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_vint8m1_t(vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_vint8m2_t(vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_m_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_m_vint8mf8_t(vbool64_t mask, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8_m(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_m_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_m_vint8mf4_t(vbool32_t mask, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4_m(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_m_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_m_vint8mf2_t(vbool16_t mask, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2_m(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_m_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_m_vint8m1_t(vbool8_t mask, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1_m(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_m_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_m_vint8m2_t(vbool4_t mask, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2_m(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_vint8mf8_t(vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_vint8mf4_t(vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_vint8mf2_t(vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_vint8m1_t(vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_vint8m2_t(vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mask_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_mask_vint8mf8_t(vbool64_t mask, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mask_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_mask_vint8mf4_t(vbool32_t mask, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mask_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_mask_vint8mf2_t(vbool16_t mask, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mask_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_mask_vint8m1_t(vbool8_t mask, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mask_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_mask_vint8m2_t(vbool4_t mask,vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf(mask, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_tu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_tu_vint8mf8_t(vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_tu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_tu_vint8mf4_t(vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_tu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_tu_vint8mf2_t(vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_tu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_tu_vint8m1_t(vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_tu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_tu_vint8m2_t(vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_tum_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_tum_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_tum_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_tum_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_tum_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_tum_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_tum_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_tum_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_tum_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_tum_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_tumu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_tumu_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_tumu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_tumu_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_tumu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_tumu_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_tumu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_tumu_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_tumu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_tumu_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf8_mu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_i8mf8_mu_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf8_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_mu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_i8mf4_mu_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf4_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf2_mu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_i8mf2_mu_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8mf2_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m1_mu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_i8m1_mu_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m1_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8m2_mu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_i8m2_mu_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_i8m2_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_tu_vint8mf8_t(vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_tu_vint8mf4_t(vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_tu_vint8mf2_t(vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_tu_vint8m1_t(vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_tu_vint8m2_t(vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tu(maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tum_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_tum_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tum_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_tum_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tum_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_tum_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tum_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_tum_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tum(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tum_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_tum_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tum(mask, maskedoff, vs2, rs1, vl);
}


/*
** test_sf_vfnrclip_x_f_qf_tumu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_tumu_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_i8mf4_tumu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_tumu_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tumu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_tumu_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tumu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_tumu_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_tumu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_tumu_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_tumu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mu_vint8mf8_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf8_t test_sf_vfnrclip_x_f_qf_mu_vint8mf8_t(vbool64_t mask, vint8mf8_t maskedoff, vfloat32mf2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mu_vint8mf4_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf4_t test_sf_vfnrclip_x_f_qf_mu_vint8mf4_t(vbool32_t mask, vint8mf4_t maskedoff, vfloat32m1_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mu_vint8mf2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8mf2_t test_sf_vfnrclip_x_f_qf_mu_vint8mf2_t(vbool16_t mask, vint8mf2_t maskedoff, vfloat32m2_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mu_vint8m1_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m1_t test_sf_vfnrclip_x_f_qf_mu_vint8m1_t(vbool8_t mask, vint8m1_t maskedoff, vfloat32m4_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_mu(mask, maskedoff, vs2, rs1, vl);
}

/*
** test_sf_vfnrclip_x_f_qf_mu_vint8m2_t:
** ...
** sf\.vfnrclip\.x\.f\.qf\tv[0-9]+,v[0-9]+,fa[0-9]+,v0.t
** ...
*/
vint8m2_t test_sf_vfnrclip_x_f_qf_mu_vint8m2_t(vbool4_t mask, vint8m2_t maskedoff, vfloat32m8_t vs2, float rs1, size_t vl) {
    return __riscv_sf_vfnrclip_x_f_qf_mu(mask, maskedoff, vs2, rs1, vl);
}
