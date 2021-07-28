/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model" } */

/* Verify the execution goes well with shift count either 32 or 48.  */

#define N 128

typedef signed int si;
typedef signed short sh;
typedef signed long long sll;
typedef unsigned int ui;
typedef unsigned short uh;
typedef unsigned long long ull;

si si_a[N], si_b[N];
ui ui_a[N], ui_b[N];
sh sh_c[N];
uh uh_c[N];

#define TEST(NTYPE, TYPE, WTYPE, CNT)                                          \
  void __attribute__ ((noipa)) test_##TYPE##CNT ()                             \
  {                                                                            \
    for (int i = 0; i < N; i++)                                                \
      NTYPE##_c[i] = ((WTYPE) TYPE##_a[i] * (WTYPE) TYPE##_b[i]) >> CNT;       \
  }                                                                            \
                                                                               \
  void __attribute__ ((noipa, optimize ("O1"))) check_##TYPE##CNT ()           \
  {                                                                            \
    test_##TYPE##CNT ();                                                       \
    for (int i = 0; i < N; i++)                                                \
      {                                                                        \
	NTYPE exp = ((WTYPE) TYPE##_a[i] * (WTYPE) TYPE##_b[i]) >> CNT;        \
	if (NTYPE##_c[i] != exp)                                               \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

TEST (sh, si, sll, 32)
TEST (sh, si, sll, 48)
TEST (uh, ui, ull, 32)
TEST (uh, ui, ull, 48)

int
main ()
{

  for (int i = 0; i < N; i++)
    {
      ui_a[i] = si_a[i] = 0x12345678ULL + 0x1000ULL * (i * 3 - 1);
      ui_b[i] = si_b[i] = 0x87654321ULL - 0x500000ULL * (i * 5 + 1);
    }

  check_si32 ();
  check_si48 ();
  check_ui32 ();
  check_ui48 ();
}
