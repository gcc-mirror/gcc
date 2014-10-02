/* PR tree-optimization/53645 */
/* { dg-options "-std=gnu89" } */

typedef unsigned short int UV __attribute__((vector_size (16)));
typedef short int SV __attribute__((vector_size (16)));
extern void abort (void);

#define TEST(a, b, c, d, e, f, g, h) \
__attribute__((noinline)) void			\
uq##a##b##c##d##e##f##g##h (UV *x, UV *y) 	\
{						\
  *x = *y / ((UV) { a, b, c, d, e, f, g, h });	\
}						\
						\
__attribute__((noinline)) void			\
ur##a##b##c##d##e##f##g##h (UV *x, UV *y) 	\
{						\
  *x = *y % ((UV) { a, b, c, d, e, f, g, h });	\
}						\
						\
__attribute__((noinline)) void			\
sq##a##b##c##d##e##f##g##h (SV *x, SV *y) 	\
{						\
  *x = *y / ((SV) { a, b, c, d, e, f, g, h });	\
}						\
						\
__attribute__((noinline)) void			\
sr##a##b##c##d##e##f##g##h (SV *x, SV *y) 	\
{						\
  *x = *y % ((SV) { a, b, c, d, e, f, g, h });	\
}

#define TESTS \
TEST (4, 4, 4, 4, 4, 4, 4, 4)		\
TEST (1, 4, 2, 8, 16, 64, 32, 128)	\
TEST (3, 3, 3, 3, 3, 3, 3, 3)		\
TEST (6, 5, 6, 5, 6, 5, 6, 5)		\
TEST (14, 14, 14, 6, 14, 6, 14, 14)	\
TEST (7, 7, 7, 7, 7, 7, 7, 7)		\

TESTS

UV u[] =
  { ((UV) { 73U, 65531U, 0U, 174U, 921U, 65535U, 17U, 178U }),
    ((UV) { 1U, 8173U, 65535U, 65472U, 12U, 29612U, 128U, 8912U }) };
SV s[] =
  { ((SV) { 73, -9123, 32761, 8191, 16371, 1201, 12701, 9999 }),
    ((SV) { 9903, -1, -7323, 0, -7, -323, 9124, -9199 }) };

int
main ()
{
  UV ur, ur2;
  SV sr, sr2;
  int i;
#undef TEST
#define TEST(a, b, c, d, e, f, g, h)			\
    uq##a##b##c##d##e##f##g##h (&ur, u + i);		\
    if (ur[0] != u[i][0] / a || ur[3] != u[i][3] / d)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[2] != u[i][2] / c || ur[1] != u[i][1] / b)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[4] != u[i][4] / e || ur[7] != u[i][7] / h)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[6] != u[i][6] / g || ur[5] != u[i][5] / f)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    ur##a##b##c##d##e##f##g##h (&ur, u + i);		\
    if (ur[0] != u[i][0] % a || ur[3] != u[i][3] % d)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[2] != u[i][2] % c || ur[1] != u[i][1] % b)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[4] != u[i][4] % e || ur[7] != u[i][7] % h)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");		\
    if (ur[6] != u[i][6] % g || ur[5] != u[i][5] % f)	\
     abort ();						\
    asm volatile ("" : : "r" (&ur) : "memory");
  for (i = 0; i < sizeof (u) / sizeof (u[0]); i++)
    {
      TESTS
    }
#undef TEST
#define TEST(a, b, c, d, e, f, g, h)			\
    sq##a##b##c##d##e##f##g##h (&sr, s + i);		\
    if (sr[0] != s[i][0] / a || sr[3] != s[i][3] / d)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[2] != s[i][2] / c || sr[1] != s[i][1] / b)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[4] != s[i][4] / e || sr[7] != s[i][7] / h)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[6] != s[i][6] / g || sr[5] != s[i][5] / f)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    sr##a##b##c##d##e##f##g##h (&sr, s + i);		\
    if (sr[0] != s[i][0] % a || sr[3] != s[i][3] % d)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[2] != s[i][2] % c || sr[1] != s[i][1] % b)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[4] != s[i][4] % e || sr[7] != s[i][7] % h)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");		\
    if (sr[6] != s[i][6] % g || sr[5] != s[i][5] % f)	\
     abort ();						\
    asm volatile ("" : : "r" (&sr) : "memory");
  for (i = 0; i < sizeof (s) / sizeof (s[0]); i++)
    {
      TESTS
    }
  return 0;
}
