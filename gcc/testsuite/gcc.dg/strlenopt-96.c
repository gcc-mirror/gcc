/* PR tree-optimization/117057 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-times "return \[0-9\]*;" 4 "optimized" { target i?86-*-* x86_64-*-* aarch64*-*-* powerpc*-*-* } } } */

#include "strlenopt.h"

typedef unsigned int V __attribute__((vector_size (2 * sizeof (int))));
typedef unsigned int W __attribute__((vector_size (4 * sizeof (int))));

size_t
foo (void)
{
  char a[64];
  *(long long *) a = 0x12003456789abcdeULL;
  return strlen (a);
}

size_t
bar (void)
{
  char a[64];
  *(V *) a = (V) { 0x12345678U, 0x9a00bcdeU };
  return strlen (a);
}

size_t
baz (unsigned int x)
{
  char a[64];
  *(V *) a = (V) { 0x12005678U, x };
  return strlen (a);
}

size_t
qux (unsigned int x)
{
  char a[64];
  *(W *)a = (W) { 0x12345678U, 0x9abcdef0U, 0x12005678U, x };
  return strlen (a);
}
