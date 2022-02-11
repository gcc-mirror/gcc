/* { dg-options "-mfloat-abi=soft" } */

#pragma GCC arm "arm_mve_types.h"  /* { dg-error {this definition requires the MVE ISA extension} } */
