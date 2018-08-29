/* PR c/83966 */
/* { dg-do compile } */
/* { dg-options "-Wsuggest-attribute=format" } */

#include "format.h"

va_list va;
const char *f;
__typeof (vsnprintf ("foo", 0U, f, va)) T;
