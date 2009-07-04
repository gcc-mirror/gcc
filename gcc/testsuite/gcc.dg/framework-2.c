/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-F$srcdir/gcc.dg" } */

#include <Foundation/Foundation.h>
/* { dg-message "terminated" "" { target *-*-* } 0 } */
/* { dg-message "Foundation/Foundation.h" "" { target *-*-* } 0 } */
