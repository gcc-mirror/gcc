/* PR target/34734 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

#include "progmem.h"

const char c PROGMEM; /* { dg-warning "uninitialized variable 'c' put into program memory area" } */
