/* PR c++/64079 */
/* { dg-do compile } */
/* { dg-options "-Wunused-function" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

static void bar() {}

#pragma GCC diagnostic pop
