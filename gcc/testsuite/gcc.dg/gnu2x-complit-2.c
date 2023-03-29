/* Test C2x storage class specifiers in compound literals.  Thread-local
   cases, compilation tests, GNU __thread used.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x" } */
/* { dg-require-effective-target tls } */

#include <stddef.h>

/* __thread is OK at file scope, although of limited use since the
   thread-local object and its address are not constant expressions.  */
size_t st = sizeof (__thread int) { 1 };
size_t sst = sizeof (static __thread int) { 1 };

int *
f ()
{
  return &(static __thread int) { 2 };
}
