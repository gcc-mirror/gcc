/* { dg-do preprocess } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */

#include <riscv_vector.h>
#include <sifive_vector.h>
#include <andes_vector.h>

#if defined (__riscv_vector) || defined (__riscv_zvabd) \
    || defined (__riscv_zvbb) \
    || defined (__riscv_zve32f) || defined (__riscv_zve32x) \
    || defined (__riscv_zve64d) || defined (__riscv_zve64f) \
    || defined (__riscv_zve64x) \
    || defined (__riscv_zvbc) || defined (__riscv_zvfbfmin) \
    || defined (__riscv_zvfbfwma) || defined (__riscv_zvfh) \
    || defined (__riscv_zvfhmin) || defined (__riscv_zvkb) \
    || defined (__riscv_zvkg) || defined (__riscv_zvkned) \
    || defined (__riscv_zvknha) || defined (__riscv_zvknhb) \
    || defined (__riscv_zvksed) || defined (__riscv_zvksh)
#error "unexpected vector ISA extension macro"
#endif

#if !defined (__riscv_intrinsic_v) \
    || !defined (__riscv_intrinsic_zve32f) \
    || !defined (__riscv_intrinsic_zve32x) \
    || !defined (__riscv_intrinsic_zve64d) \
    || !defined (__riscv_intrinsic_zve64f) \
    || !defined (__riscv_intrinsic_zve64x) \
    || !defined (__riscv_intrinsic_zvabd) \
    || !defined (__riscv_intrinsic_zvbb) \
    || !defined (__riscv_intrinsic_zvbc) \
    || !defined (__riscv_intrinsic_zvfbfmin) \
    || !defined (__riscv_intrinsic_zvfbfwma) \
    || !defined (__riscv_intrinsic_zvfh) \
    || !defined (__riscv_intrinsic_zvfhmin) \
    || !defined (__riscv_intrinsic_zvkb) \
    || !defined (__riscv_intrinsic_zvkg) \
    || !defined (__riscv_intrinsic_zvkn) \
    || !defined (__riscv_intrinsic_zvknc) \
    || !defined (__riscv_intrinsic_zvkned) \
    || !defined (__riscv_intrinsic_zvkng) \
    || !defined (__riscv_intrinsic_zvknha) \
    || !defined (__riscv_intrinsic_zvknhb) \
    || !defined (__riscv_intrinsic_zvks) \
    || !defined (__riscv_intrinsic_zvksc) \
    || !defined (__riscv_intrinsic_zvksed) \
    || !defined (__riscv_intrinsic_zvksg) \
    || !defined (__riscv_intrinsic_zvksh) \
    || !defined (__riscv_intrinsic_xsfvcp) \
    || !defined (__riscv_intrinsic_xsfvfnrclipxfqf) \
    || !defined (__riscv_intrinsic_xsfvqmaccdod) \
    || !defined (__riscv_intrinsic_xsfvqmaccqoq) \
    || !defined (__riscv_intrinsic_xandesvbfhcvt) \
    || !defined (__riscv_intrinsic_xandesvdot) \
    || !defined (__riscv_intrinsic_xandesvpackfph) \
    || !defined (__riscv_intrinsic_xandesvsintload)
#error "missing vector intrinsic detection macro"
#endif

#if __riscv_intrinsic_v != 1 \
    || __riscv_intrinsic_zve32f != 1 \
    || __riscv_intrinsic_zve32x != 1 \
    || __riscv_intrinsic_zve64d != 1 \
    || __riscv_intrinsic_zve64f != 1 \
    || __riscv_intrinsic_zve64x != 1 \
    || __riscv_intrinsic_zvabd != 1 \
    || __riscv_intrinsic_zvbb != 1 \
    || __riscv_intrinsic_zvbc != 1 \
    || __riscv_intrinsic_zvfbfmin != 1 \
    || __riscv_intrinsic_zvfbfwma != 1 \
    || __riscv_intrinsic_zvfh != 1 \
    || __riscv_intrinsic_zvfhmin != 1 \
    || __riscv_intrinsic_zvkb != 1 \
    || __riscv_intrinsic_zvkg != 1 \
    || __riscv_intrinsic_zvkn != 1 \
    || __riscv_intrinsic_zvknc != 1 \
    || __riscv_intrinsic_zvkned != 1 \
    || __riscv_intrinsic_zvkng != 1 \
    || __riscv_intrinsic_zvknha != 1 \
    || __riscv_intrinsic_zvknhb != 1 \
    || __riscv_intrinsic_zvks != 1 \
    || __riscv_intrinsic_zvksc != 1 \
    || __riscv_intrinsic_zvksed != 1 \
    || __riscv_intrinsic_zvksg != 1 \
    || __riscv_intrinsic_zvksh != 1 \
    || __riscv_intrinsic_xsfvcp != 1 \
    || __riscv_intrinsic_xsfvfnrclipxfqf != 1 \
    || __riscv_intrinsic_xsfvqmaccdod != 1 \
    || __riscv_intrinsic_xsfvqmaccqoq != 1 \
    || __riscv_intrinsic_xandesvbfhcvt != 1 \
    || __riscv_intrinsic_xandesvdot != 1 \
    || __riscv_intrinsic_xandesvpackfph != 1 \
    || __riscv_intrinsic_xandesvsintload != 1
#error "bad vector intrinsic detection macro value"
#endif
