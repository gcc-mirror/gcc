/* Various Solaris versions have been known to have problems with the
 * pthread initialization macros when __STDC__ is defined.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

/* { dg-do compile } */
/* { dg-options "-Wextra -Wall -ansi" } */

#include "pthread-init-common.h"

