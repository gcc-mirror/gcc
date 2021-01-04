/* Ensure we get clean warning results when using the pthread
 * initialization macros.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

/* { dg-do compile } */
/* { dg-require-effective-target pthread_h } */
/* { dg-options "-Wextra -Wall" } */
/* The definition of PTHREAD_MUTEX_INITIALIZER is missing an initializer for
   mutexAttr.mutexAttrType in kernel mode for various VxWorks versions.  */
/* { dg-xfail-if "missing initializer" { vxworks_kernel } } */

#include "pthread-init-common.h"

