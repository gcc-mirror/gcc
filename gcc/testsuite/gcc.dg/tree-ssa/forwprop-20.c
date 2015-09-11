/* { dg-do compile } */
/* { dg-require-effective-target double64 } */
/* { dg-options "-O -fdump-tree-forwprop1" }  */

#include <stdint.h>

/* All of these optimizations happen for unsupported vector modes as a
   consequence of the lowering pass. We need to test with a vector mode
   that is supported by default on at least some architectures, or make
   the test target specific so we can pass a flag like -mavx.  */

typedef double vecf __attribute__ ((vector_size (2 * sizeof (double))));
typedef int64_t veci __attribute__ ((vector_size (2 * sizeof (int64_t))));

void f (double d, vecf* r)
{
  vecf x = { -d, 5 };
  vecf y = {  1, 4 };
  veci m = {  2, 0 };
  *r = __builtin_shuffle (x, y, m); // { 1, -d }
}

void g (float d, vecf* r)
{
  vecf x = { d, 5 };
  vecf y = { 1, 4 };
  veci m = { 2, 1 };
  *r = __builtin_shuffle (x, y, m); // { 1, 5 }
}

void h (double d, vecf* r)
{
  vecf x = { d + 1, 5 };
  vecf y = {   1  , 4 };
  veci m = {   2  , 0 };
  *r = __builtin_shuffle (y, x, m); // { d + 1, 1 }
}

void i (float d, vecf* r)
{
  vecf x = { d, 5 };
  veci m = { 1, 0 };
  *r = __builtin_shuffle (x, m); // { 5, d }
}

void j (vecf* r)
{
  vecf y = {  1, 2 };
  veci m = {  0, 0 };
  *r = __builtin_shuffle (y, m); // { 1, 1 }
}

void k (vecf* r)
{
  vecf x = {  3, 4 };
  vecf y = {  1, 2 };
  veci m = {  3, 0 };
  *r = __builtin_shuffle (x, y, m); // { 2, 3 }
}

void l (double d, vecf* r)
{
  vecf x = { -d, 5 };
  vecf y = {  d, 4 };
  veci m = {  2, 0 };
  *r = __builtin_shuffle (x, y, m); // { d, -d }
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "forwprop1" } } */
