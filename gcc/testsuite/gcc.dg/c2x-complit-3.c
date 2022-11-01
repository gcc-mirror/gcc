/* Test C2x storage class specifiers in compound literals.  Thread-local
   cases, compilation tests.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */
/* { dg-require-effective-target tls } */

#include <stddef.h>

/* thread_local is OK at file scope, although of limited use since the
   thread-local object and its address are not constant expressions.  */
size_t st = sizeof (thread_local int) { 1 };
size_t sst = sizeof (static thread_local int) { 1 };

int *
f ()
{
  return &(static thread_local int) { 2 };
}

int *
g ()
{
  return &(thread_local static int) { 3 };
}
