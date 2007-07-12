/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-F$srcdir/gcc.dg" } */

#include <Foundation/Foundation.h> /* { dg-error "Foundation/Foundation.h: No such file" } */
