/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h"

struct svpattern { int x; }; /* { dg-error "'svpattern' referred to as 'struct'" } */
