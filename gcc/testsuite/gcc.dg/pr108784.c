/* PR debug/108784 */
/* { dg-do compile } */
/* { dg-options "-O2 -w --param=ira-simple-lra-insn-threshold=1 -fcompare-debug" } */

extern unsigned external_mix (unsigned);

/* These unused named locals generate debug insns without affecting code.  */
#define GHOST(P, I, N) unsigned ghost##P##I = x ^ (N) ^ (I)
#define GHOSTS_8(P, N) \
  GHOST (P, 0, N); GHOST (P, 1, N); GHOST (P, 2, N); GHOST (P, 3, N); \
  GHOST (P, 4, N); GHOST (P, 5, N); GHOST (P, 6, N); GHOST (P, 7, N)
#define GHOSTS_64(N) \
  GHOSTS_8 (a, N); GHOSTS_8 (b, N); GHOSTS_8 (c, N); GHOSTS_8 (d, N); \
  GHOSTS_8 (e, N); GHOSTS_8 (f, N); GHOSTS_8 (g, N); GHOSTS_8 (h, N)

#define STEP(N, A, I, C) \
  do \
    { \
      GHOSTS_64 (N); \
      A = A * 33u + x + p[I]; \
      x = x * 1103515245u + A + (C); \
      __asm__ volatile ("" : "+r" (x)); \
    } \
  while (0)

#define UNIT() \
  STEP (1u, a0, 0, 12345u); \
  STEP (2u, a1, 1, 12346u); \
  STEP (3u, a2, 2, 12347u); \
  STEP (4u, a3, 3, 12348u); \
  STEP (5u, a4, 4, 12349u); \
  STEP (6u, a5, 5, 12350u); \
  STEP (7u, a6, 6, 12351u); \
  STEP (8u, a7, 7, 12352u); \
  STEP (9u, a8, 0, 12353u); \
  STEP (10u, a9, 1, 12354u); \
  STEP (11u, a10, 2, 12355u); \
  STEP (12u, a11, 3, 12356u); \
  if ((x & 31u) == 7u) x ^= external_mix (x);

#define REP_1() UNIT()
#define REP_2() REP_1() REP_1()

__attribute__((noinline))
unsigned
foo (unsigned x, const unsigned *p)
{
  unsigned a0 = x + p[0] + 1u;
  unsigned a1 = x + p[1] + 2u;
  unsigned a2 = x + p[2] + 3u;
  unsigned a3 = x + p[3] + 4u;
  unsigned a4 = x + p[4] + 5u;
  unsigned a5 = x + p[5] + 6u;
  unsigned a6 = x + p[6] + 7u;
  unsigned a7 = x + p[7] + 8u;
  unsigned a8 = x + p[0] + 9u;
  unsigned a9 = x + p[1] + 10u;
  unsigned a10 = x + p[2] + 11u;
  unsigned a11 = x + p[3] + 12u;
  REP_2 ();
  return x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
         + a11;
}
