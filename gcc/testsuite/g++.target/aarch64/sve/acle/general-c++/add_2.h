#pragma GCC system_header
#pragma GCC aarch64 "arm_sve.h"
/* { dg-message {note: candidate: 'svfloat16_t svadd_x\(svbool_t, svfloat16_t, svfloat16_t\)'} "" { target *-*-* } 3 } */
/* { dg-message {note: *candidate expects 3 arguments, 2 provided} "" { target *-*-* } 3 } */
/* { dg-message {note: *candidate expects 3 arguments, 4 provided} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 1 from 'svuint8_t' to 'svbool_t'} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 2 from 'svbool_t' to 'svfloat16_t'} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 2 from 'int' to 'svfloat16_t'} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 2 from 'svuint8_t' to 'svfloat16_t'} "" { target *-*-* } 3 } */
