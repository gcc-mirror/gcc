/* Ensure we get clean warning results when using the pthread
 * initialization macros.
 *
 * Origin: Kaveh Ghazi (ghazi@caip.rutgers.edu) 9/27/2006.
 */

/* { dg-do compile } */
/* { dg-require-effective-target pthread_h } */
/* { dg-options "-Wextra -Wall" } */

#include "pthread-init-common.h"

