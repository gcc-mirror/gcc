/* { dg-require-effective-target run_expensive_tests } */
/* { dg-timeout-factor 8 } */
/* { dg-skip-if "memory full + time hog" { "avr-*-*" } } */

#define ax_t a4_t
#include "memcpy-ax.h"
