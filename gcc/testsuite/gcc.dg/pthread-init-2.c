/* Various Solaris versions have been known to have problems with the
 * pthread initialization macros when __STDC__ is defined.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

/* { dg-do compile } */
/* { dg-require-effective-target pthread_h } */
/* { dg-options "-Wextra -Wall -ansi" } */
/* { dg-options "-Wextra -Wall -ansi -D_POSIX_C_SOURCE=199506L" { target { *-*-hpux* } } } */
/* { dg-options "-Wextra -Wall -ansi -D_XOPEN_SOURCE=500" { target { powerpc-ibm-aix* } } } */
/* The definition of PTHREAD_MUTEX_INITIALIZER is missing an initializer for
   mutexAttr.mutexAttrType in kernel mode for various VxWorks versions.  */
/* { dg-xfail-if "missing initializer" { vxworks_kernel } } */

#include "pthread-init-common.h"

