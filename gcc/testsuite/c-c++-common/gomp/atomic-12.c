/* PR middle-end/45423 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple -g0 -Wno-deprecated" } */
/* atomicvar should never be referenced in between the barrier and
   following #pragma omp atomic_load.  */
/* { dg-final { scan-tree-dump-not "barrier\[^#\]*atomicvar" "gimple" } } */
/* { dg-skip-if "invalid in C++17" { c++17 } } */

#ifdef __cplusplus
bool atomicvar, c;
#else
_Bool atomicvar, c;
#endif
int i, atomicvar2, c2;

int
foo (void)
{
  #pragma omp barrier
  #pragma omp atomic
    atomicvar |= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar |= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar |= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar |= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar |= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar ^= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar ^= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar ^= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar ^= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar ^= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar &= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar &= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar &= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar &= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar &= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar += -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar += 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar += 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar += 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar += c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar -= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar -= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar -= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar -= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar -= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar *= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar *= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar *= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar *= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar *= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar /= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar /= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar /= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar /= c;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar <<= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar <<= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar <<= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar <<= i;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar >>= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar >>= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar >>= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar >>= i;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar++;
  #pragma omp barrier
  #pragma omp atomic
    ++atomicvar;
  #pragma omp barrier
#ifndef __cplusplus
  #pragma omp atomic
    atomicvar--;
  #pragma omp barrier
  #pragma omp atomic
    --atomicvar;
  #pragma omp barrier
#endif
  return 0;
}

int
bar (void)
{
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 |= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 |= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 |= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 |= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 |= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 ^= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 ^= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 ^= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 ^= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 ^= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 &= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 &= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 &= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 &= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 &= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 += -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 += 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 += 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 += 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 += c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 -= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 -= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 -= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 -= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 -= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 *= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 *= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 *= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 *= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 *= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 /= -1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 /= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 /= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 /= c2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 <<= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 <<= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 <<= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 <<= i;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 >>= 0;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 >>= 1;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 >>= 2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2 >>= i;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2++;
  #pragma omp barrier
  #pragma omp atomic
    ++atomicvar2;
  #pragma omp barrier
  #pragma omp atomic
    atomicvar2--;
  #pragma omp barrier
  #pragma omp atomic
    --atomicvar2;
  #pragma omp barrier
  return 0;
}
