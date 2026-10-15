/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */

/* Two loop switches in one function.  Each continuing handler takes a copy
   of the dispatch of its loop, so all the copies of a loop share its table.
   Every run executes two instructions and a halt.  So the copy at the loop
   entry dispatches every value, and the copy in each handler dispatches
   every case, the hole, both ends of the range and both limits of the index
   type.  A trace and an accumulator that depends on the order check the
   exact path.  With LARGE, two handlers get about 2048 stores each, so some
   copies are far from the table.  */

#ifdef LARGE
#define S1(k) sd_sink[(k) & 7] = (unsigned int) ((k) * 0x9e3779b1u);
#define S4(k) S1 (k) S1 ((k) + 1) S1 ((k) + 2) S1 ((k) + 3)
#define S16(k) S4 (k) S4 ((k) + 4) S4 ((k) + 8) S4 ((k) + 12)
#define S64(k) S16 (k) S16 ((k) + 16) S16 ((k) + 32) S16 ((k) + 48)
#define S512(k) S64 (k) S64 ((k) + 64) S64 ((k) + 128) S64 ((k) + 192) \
		S64 ((k) + 256) S64 ((k) + 320) S64 ((k) + 384) S64 ((k) + 448)
#define BIG(k) S512 (k) S512 ((k) + 512) S512 ((k) + 1024) S512 ((k) + 1536)
#else
#define BIG(k)
#endif

#define SEED 0x2a5u

volatile unsigned int sd_sink[8];
volatile unsigned char sd_trace[8];

__attribute__ ((noipa)) unsigned int
sd_bump (unsigned int x)
{
  return x * 9 + 4;
}

static inline unsigned int
sd_spin (unsigned int x)
{
  unsigned int n = (x & 3) + 1;
  do
    x = x * 5 + n;
  while (--n);
  return x;
}

/* Case value, trace number, new accumulator and extra handler code.  */
#define A_CASES(C, C2)					\
  C (-4, 1, acc * 3 + 1, BIG (0))			\
  C (-3, 2, acc ^ 0x5a5, )				\
  C (-2, 3, acc - 77, )					\
  C (-1, 4, (acc << 3) | (acc >> 11), )			\
  C (1, 5, acc + 1001, )				\
  C (2, 6, acc * 7, )					\
  C (3, 7, ~acc, )					\
  C (4, 8, sd_bump (acc), )				\
  C (6, 9, acc ^ (acc >> 2), )				\
  C (7, 10, sd_spin (acc), )				\
  C2 (8, 9, 11, acc * 11 + 5, BIG (2048))
#define A_HALT 0
#define A_DEF_ID 12
#define A_DEF(acc) ((acc) * 13 + 12)

#define B_CASES(C, C2)					\
  C (1000, 13, acc + 3, )				\
  C (1001, 14, acc * 5 + 2, )				\
  C (1002, 15, acc ^ 0x3c3, )				\
  C (1003, 16, sd_spin (acc), )				\
  C (1004, 17, acc - (acc >> 3), )			\
  C (1005, 18, sd_bump (acc) + 1, )			\
  C (1006, 19, (acc >> 5) | (acc << 7), )		\
  C (1008, 20, acc * 17, )				\
  C (1009, 21, acc + 0x777, )				\
  C (1010, 22, ~acc + 3, )				\
  C (1011, 23, acc ^ (acc << 1), )			\
  C2 (1012, 1013, 24, acc * 19 + 7, )			\
  C (1014, 25, acc - 1015, )
#define B_HALT 1015
#define B_DEF_ID 26
#define B_DEF(acc) ((acc) * 29 + 26)

#define CASE(V, ID, E, X) \
  case V: X sd_trace[n++ & 7] = ID; acc = E; break;
#define CASE2(V1, V2, ID, E, X) \
  case V1: case V2: X sd_trace[n++ & 7] = ID; acc = E; break;

__attribute__ ((noipa)) unsigned int
interp (const signed char *pa, const int *pb, unsigned int *len)
{
  unsigned int acc = SEED, n = 0;

  for (;;)
    switch (*pa++)
      {
      A_CASES (CASE, CASE2)
      case A_HALT:
	goto second;
      default:
	sd_trace[n++ & 7] = A_DEF_ID;
	acc = A_DEF (acc);
	break;
      }

 second:
  for (;;)
    switch (*pb++)
      {
      B_CASES (CASE, CASE2)
      case B_HALT:
	*len = n;
	return acc;
      default:
	sd_trace[n++ & 7] = B_DEF_ID;
	acc = B_DEF (acc);
	break;
      }
}

#define REF(V, ID, E, X) if (v == (V)) { *id = ID; return E; }
#define REF2(V1, V2, ID, E, X) \
  if (v == (V1) || v == (V2)) { *id = ID; return E; }

__attribute__ ((noipa)) static unsigned int
ref_a (int v, unsigned int acc, unsigned char *id)
{
  A_CASES (REF, REF2)
  *id = A_DEF_ID;
  return A_DEF (acc);
}

__attribute__ ((noipa)) static unsigned int
ref_b (int v, unsigned int acc, unsigned char *id)
{
  B_CASES (REF, REF2)
  *id = B_DEF_ID;
  return B_DEF (acc);
}

static const signed char a_values[] = {
  -__SCHAR_MAX__ - 1, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  __SCHAR_MAX__
};

static const int b_values[] = {
  -__INT_MAX__ - 1, -1, 0, 999, 1000, 1001, 1002, 1003, 1004, 1005, 1006,
  1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, __INT_MAX__
};

static void
check_a (signed char i, signed char j)
{
  const signed char pa[3] = { i, j, A_HALT };
  static const int pb[1] = { B_HALT };
  unsigned char want[2];
  unsigned int acc = SEED, n = 0, len;

  for (unsigned int k = 0; k < 2 && pa[k] != A_HALT; k++, n++)
    acc = ref_a (pa[k], acc, &want[n]);
  if (interp (pa, pb, &len) != acc || len != n)
    __builtin_abort ();
  for (unsigned int k = 0; k < n; k++)
    if (sd_trace[k] != want[k])
      __builtin_abort ();
}

static void
check_b (int i, int j)
{
  static const signed char pa[1] = { A_HALT };
  const int pb[3] = { i, j, B_HALT };
  unsigned char want[2];
  unsigned int acc = SEED, n = 0, len;

  for (unsigned int k = 0; k < 2 && pb[k] != B_HALT; k++, n++)
    acc = ref_b (pb[k], acc, &want[n]);
  if (interp (pa, pb, &len) != acc || len != n)
    __builtin_abort ();
  for (unsigned int k = 0; k < n; k++)
    if (sd_trace[k] != want[k])
      __builtin_abort ();
}

int
main (void)
{
  for (unsigned int i = 0; i < sizeof a_values; i++)
    for (unsigned int j = 0; j < sizeof a_values; j++)
      check_a (a_values[i], a_values[j]);
  for (unsigned int i = 0; i < sizeof b_values / sizeof b_values[0]; i++)
    for (unsigned int j = 0; j < sizeof b_values / sizeof b_values[0]; j++)
      check_b (b_values[i], b_values[j]);
  return 0;
}
