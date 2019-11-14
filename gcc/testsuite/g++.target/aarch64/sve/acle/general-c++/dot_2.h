#pragma GCC system_header
#pragma GCC aarch64 "arm_sve.h"
/* { dg-message {note: candidate: 'svuint32_t svdot\(svuint32_t, svuint8_t, svuint8_t\)'} "" { target *-*-* } 3 } */
/* { dg-message {note: *candidate expects 3 arguments, 2 provided} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 2 from 'svuint32_t' to 'svuint8_t'} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 1 from 'int' to 'svuint32_t'} "" { target *-*-* } 3 } */
/* { dg-message {note: *no known conversion for argument 2 from 'svint8_t' to 'svuint8_t'} "" { target *-*-* } 3 } */
