/* { dg-do compile } */

__attribute__ ((aarch64_vector_pcs)) void f1 (__SVBool_t); /* { dg-error {the 'aarch64_vector_pcs' attribute cannot be applied to an SVE function type} } */
__attribute__ ((aarch64_vector_pcs)) void f2 (__SVInt8_t s8) {} /* { dg-error {the 'aarch64_vector_pcs' attribute cannot be applied to an SVE function type} } */
__attribute__ ((aarch64_vector_pcs)) void (*f3) (__SVInt16_t); /* { dg-error {the 'aarch64_vector_pcs' attribute cannot be applied to an SVE function type} } */
typedef __attribute__ ((aarch64_vector_pcs)) void (*f4) (__SVInt32_t); /* { dg-error {the 'aarch64_vector_pcs' attribute cannot be applied to an SVE function type} } */
