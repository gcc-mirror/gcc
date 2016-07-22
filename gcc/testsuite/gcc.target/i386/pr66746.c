/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -miamcu" } */
/* { dg-add-options bind_pic_locally } */

/* Defining away "extern" and "__inline" results in all of them being
   compiled as proper functions.  */

#define extern
#define __inline

#include <x86intrin.h>
