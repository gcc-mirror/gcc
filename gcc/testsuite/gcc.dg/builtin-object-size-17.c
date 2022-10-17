/* PR 71831 - __builtin_object_size poor results with no optimization
   Verify that even without optimization __builtin_object_size result
   is folded into a constant and dead code that depends on it is
   eliminated.  */
/* { dg-do compile } */
/* { dg-options "-O0 -fdump-tree-ssa" } */

#define concat(a, b)   a ## b
#define CAT(a, b)      concat (a, b)

/* Create a symbol name unique to each tes and object size type.  */
#define SYM(type)      CAT (CAT (CAT (failure_on_line_, __LINE__), _type_), type)

/* References to the following undefined symbol which is unique for each
   test case are expected to be eliminated.  */
#define TEST_FAILURE(type)			\
  do {						\
    extern void SYM (type)(void);		\
    SYM (type)();				\
  } while (0)

#define bos(obj, type) __builtin_object_size (obj, type)
#define size(obj, n) ((size_t)n == X ? sizeof *obj : (size_t)n)

#define test(expect, type, obj)			\
  do {						\
    if (bos (obj, type)	!= size (obj, expect))	\
      TEST_FAILURE (type);			\
  } while (0)

#define FOLD_ALL(r0, r1, r2, r3, obj)		\
  do {						\
    test (r0, 0, obj);				\
    test (r1, 1, obj);				\
    test (r2, 2, obj);				\
    test (r3, 3, obj);				\
  } while (0)

#define FOLD_0_2(r0, r1, r2, r3, obj)		\
  do {						\
    test (r0, 0, obj);				\
    test (r2, 2, obj);				\
  } while (0)

/* For convenience.  Substitute for 'sizeof object' in test cases where
   the size can vary from target to target.  */
#define X  (size_t)0xdeadbeef

typedef __SIZE_TYPE__ size_t;

extern char ax[];
#ifndef __builtin_object_size
char ax2[];               /* { dg-warning "assumed to have one element" } */
#endif

extern char a0[0];
static char a1[1];
static char a2[2];
static char a9[9];

#if __SIZEOF_SHORT__ == 4
extern short ia0[0];
static short ia1[1];
static short ia9[9];
#elif __SIZEOF_INT__ == 4
extern int ia0[0];
static int ia1[1];
static int ia9[9];
#elif __SIZEOF_LONG__ == 4
extern long ia0[0];
static long ia1[1];
static long ia9[9];
#endif

static char a2x2[2][2];
static char a3x5[3][5];

struct Sx { char n, a[]; } sx;
struct S0 { char n, a[0]; } s0;
struct S1 { char n, a[1]; } s1;
struct S2 { char n, a[2]; } s2;
struct S9 { char n, a[9]; } s9;

struct S2x2 { char n, a[2][2]; } s2x2;
struct S3x5 { char n, a[3][5]; } s3x5;

static __attribute__ ((noclone, noinline)) void
test_arrays ()
{
  FOLD_ALL (     1,       1,       1,       1,   ax2);

  FOLD_ALL (     1,       1,       1,       1,   a1);
  FOLD_ALL (     2,       2,       2,       2,   a2);
  FOLD_ALL (     9,       9,       9,       9,   a9);

  FOLD_ALL (     0,       0,       0,       0,   a0);
  FOLD_ALL (     1,       1,       1,       1,   ax2);

  FOLD_ALL (     0,       0,       0,       0,   ia0);
  FOLD_ALL (     4,       4,       4,       4,   ia1);
  FOLD_ALL (    36,      36,      36,      36,   ia9);

  /* Not all results for multidimensional arrays make sense (see
     bug 77293).  The expected results below simply reflect those
     obtained at -O2 (modulo the known limitations at -O1).  */
  FOLD_ALL (     4,       4,       4,       4,   a2x2);
  FOLD_ALL (     4,       4,       4,       4,   &a2x2[0]);
  FOLD_ALL (     4,       2,       4,       2,   &a2x2[0][0]);
  FOLD_0_2 (     0,  F1  (0),      0,       0,   &a2x2 + 1);
  FOLD_0_2 (     2,  F1 ( 2),      2,  F3 ( 2),  &a2x2[0] + 1);
  FOLD_0_2 (     3,  F1 ( 1),      3,  F3 ( 3),  &a2x2[0][0] + 1);

  FOLD_ALL (    15,      15,      15,      15,   a3x5);
  FOLD_ALL (    15,       5,      15,       5,   &a3x5[0][0] + 0);
  FOLD_0_2 (    14,  F1 ( 4),     14,  F3 (14),  &a3x5[0][0] + 1);

  FOLD_ALL (     1,       1,       1,       1,   a1 + 0);
  FOLD_0_2 (     0,  F1 ( 0),      0,       0,   &a1 + 1);
  FOLD_ALL (     2,       2,       2,       2,   a2 + 0);
  FOLD_0_2 (     1,  F1 ( 1),      1, F3 ( 1),   a2 + 1);
  FOLD_0_2 (     0,  F1 ( 0),      0,       0,   a2 + 2);
}

static __attribute__ ((noclone, noinline)) void
test_structs (void)
{
  /* The expected size of a declared object with a flexible array member
     is sizeof sx in all __builtin_object_size types.  */
  FOLD_ALL (     X,       X,       X,       X,   &sx);

  /* The expected size of a flexible array member of a declared object
     is zero.  */
  FOLD_ALL (     0,       0,       0,       0,   sx.a);

  /* The expected size of a declared object with a zero-length array member
     is sizeof sx in all __builtin_object_size types.  */
  FOLD_ALL (     X,       X,       X,       X,   &s0);

  /* The expected size of a zero-length array member of a declared object
     is zero.  */
  FOLD_ALL (     0,       0,       0,       0,   s0.a);

  FOLD_ALL (     X,       X,       X,       X,   &s1);
  FOLD_ALL (     1,       1,       1,       1,   s1.a);
  FOLD_0_2 (     0,  F1 (0),       0,       0,   s1.a + 1);

  FOLD_ALL (     X,       X,       X,       X,   &s9);
  FOLD_ALL (     9,       9,       9,       9,   s9.a);
  FOLD_ALL (     9,       9,       9,       9,   s9.a + 0);
  FOLD_0_2 (     8,  F1 ( 8),      8, F3 (  8),  s9.a + 1);
  FOLD_0_2 (     7,  F1 ( 7),      7, F3 (  7),  s9.a + 2);
  FOLD_0_2 (     0,  F1 ( 0),      0, F3 (  0),  s9.a + 9);
}

int
main()
{
  test_arrays ();
  test_structs ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "ssa" } } */
