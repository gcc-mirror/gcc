/* PR target/44338 */
/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration -march=k8 -ffp-contract=off" } */
/* { dg-add-options bind_pic_locally } */

#include "sse-23.c"
