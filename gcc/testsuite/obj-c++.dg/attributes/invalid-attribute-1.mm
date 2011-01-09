/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, January 2011.  */
/* { dg-do compile } */

#include <objc/objc.h>

__attribute__ ((deprecated)) @class A; /* { dg-error "attributes may not be specified before the ..class. Objective-C.. keyword" } */
