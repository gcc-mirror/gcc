// { dg-compile }
// { dg-additional-options "-msve-vector-bits=512" }

#include <arm_sve.h>

typedef svint8_t vec8 __attribute__((arm_sve_vector_bits(512)));
typedef vec8 *vec8_ptr;

typedef svint8_t my_vec;

typedef vec8 bad_vec8_a __attribute__((arm_sve_vector_bits(512))); // { dg-error {'arm_sve_vector_bits' applied to type 'vec8' {aka 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)'}, which already has a size} }
typedef svint8_t bad_vec8_b __attribute__((arm_sve_vector_bits(512))) __attribute__((arm_sve_vector_bits(512))); // { dg-error {'arm_sve_vector_bits' applied to type 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)', which already has a size} }

svint8_t *vla1;
__SVInt8_t *vla2;

vec8 *vls1;
svint8_t (__attribute__((arm_sve_vector_bits(512))) *vls2);
__SVInt8_t (__attribute__((arm_sve_vector_bits(512))) *vls3);
vec8_ptr vls4;
my_vec (__attribute__((arm_sve_vector_bits(512))) *vls5);

void
f (void)
{
  vls1 = vla1; // { dg-error {invalid conversion from 'svint8_t\*' to 'vec8\*' {aka 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'}} }
  vls1 = vla2; // { dg-error {invalid conversion from '__SVInt8_t\*' to 'vec8\*' {aka 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'}} }

  vls2 = vla1; // { dg-error {invalid conversion from 'svint8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }
  vls2 = vla2; // { dg-error {invalid conversion from '__SVInt8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }

  vls3 = vla1; // { dg-error {invalid conversion from 'svint8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }
  vls3 = vla2; // { dg-error {invalid conversion from '__SVInt8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }

  vls4 = vla1; // { dg-error {invalid conversion from 'svint8_t\*' to 'vec8_ptr' {aka 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'}} }
  vls4 = vla2; // { dg-error {invalid conversion from '__SVInt8_t\*' to 'vec8_ptr' {aka 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'}} }

  vls5 = vla1; // { dg-error {invalid conversion from 'svint8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }
  vls5 = vla2; // { dg-error {invalid conversion from '__SVInt8_t\*' to 'svint8_t __attribute__\(\(arm_sve_vector_bits\(512\)\)\)\*'} }

  vla1 = vla1;
  vla1 = vla2;

  vla2 = vla1;
  vla2 = vla2;

  vls1 = vls1;
  vls1 = vls2;
  vls1 = vls3;
  vls1 = vls4;

  vls2 = vls1;
  vls2 = vls2;
  vls2 = vls3;
  vls2 = vls4;

  vls3 = vls1;
  vls3 = vls2;
  vls3 = vls3;
  vls3 = vls4;

  vls4 = vls1;
  vls4 = vls2;
  vls4 = vls3;
  vls4 = vls4;
}
