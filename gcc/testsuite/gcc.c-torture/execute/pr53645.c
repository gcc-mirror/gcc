/* PR tree-optimization/53645 */

typedef unsigned int UV __attribute__((vector_size (16)));
typedef int SV __attribute__((vector_size (16)));
extern void abort (void);

#define TEST(a, b, c, d) \
__attribute__((noinline)) void		\
uq##a##b##c##d (UV *x, UV *y) 		\
{					\
  *x = *y / ((UV) { a, b, c, d });	\
}					\
					\
__attribute__((noinline)) void		\
ur##a##b##c##d (UV *x, UV *y) 		\
{					\
  *x = *y % ((UV) { a, b, c, d });	\
}					\
					\
__attribute__((noinline)) void		\
sq##a##b##c##d (SV *x, SV *y) 		\
{					\
  *x = *y / ((SV) { a, b, c, d });	\
}					\
					\
__attribute__((noinline)) void		\
sr##a##b##c##d (SV *x, SV *y) 		\
{					\
  *x = *y % ((SV) { a, b, c, d });	\
}

#define TESTS \
TEST (4, 4, 4, 4)			\
TEST (1, 4, 2, 8)			\
TEST (3, 3, 3, 3)			\
TEST (6, 5, 6, 5)			\
TEST (14, 14, 14, 6)			\
TEST (7, 7, 7, 7)			\

TESTS

UV u[] =
  { ((UV) { 73U, 65531U, 0U, 174U }),
    ((UV) { 1U, 8173U, ~0U, ~0U - 63 }) };
SV s[] =
  { ((SV) { 73, -9123, 32761, 8191 }),
    ((SV) { 9903, -1, -7323, 0 }) };

int
main ()
{
  UV ur, ur2;
  SV sr, sr2;
  int i;
#undef TEST
#define TEST(a, b, c, d)				\
    uq##a##b##c##d (&ur, u + i);			\
    if (ur[0] != u[i][0] / a || ur[3] != u[i][3] / d)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[2] != u[i][2] / c || ur[1] != u[i][1] / b)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    ur##a##b##c##d (&ur, u + i);			\
    if (ur[0] != u[i][0] % a || ur[3] != u[i][3] % d)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[2] != u[i][2] % c || ur[1] != u[i][1] % b)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");
  for (i = 0; i < sizeof (u) / sizeof (u[0]); i++)
    {
      TESTS
    }
#undef TEST
#define TEST(a, b, c, d)				\
    sq##a##b##c##d (&sr, s + i);			\
    if (sr[0] != s[i][0] / a || sr[3] != s[i][3] / d)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[2] != s[i][2] / c || sr[1] != s[i][1] / b)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    sr##a##b##c##d (&sr, s + i);			\
    if (sr[0] != s[i][0] % a || sr[3] != s[i][3] % d)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[2] != s[i][2] % c || sr[1] != s[i][1] % b)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");
  for (i = 0; i < sizeof (s) / sizeof (s[0]); i++)
    {
      TESTS
    }
  return 0;
}
