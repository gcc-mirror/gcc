/* PR tree-optimization/47477 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -w" } */
/* { dg-require-effective-target ilp32 } */

typedef int int64_t __attribute__ ((__mode__ (__DI__)));
typedef int * intptr_t;

typedef struct toto_s *toto_t;
toto_t add (toto_t a, toto_t b) {
  int64_t tmp = (int64_t)(intptr_t)a + ((int64_t)(intptr_t)b&~1L);
  return (toto_t)(intptr_t) tmp;
}

/* For an ILP32 target there'll be 6 casts when we start, but just 4
   if the match.pd pattern is successfully matched.  */
/* { dg-final { scan-tree-dump-times "= \\(int\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \\(unsigned int\\)" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= \\(struct toto_s \\*\\)" 1 "optimized" } } */


