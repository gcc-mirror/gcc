/* PR middle-end/126236 */
/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-options "-O2 -fchecking -fduplicate-switch-dispatch" } */

#define TEST_NAME valid
#include "tablejump-test.h"
