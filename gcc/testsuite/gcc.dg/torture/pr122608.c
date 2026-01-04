/* { dg-do compile } */
/* { dg-options "-fgimple -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */

#define F(OP,NAME) \
 __GIMPLE int NAME##_test (int a) \
 { _Bool b; \
   int t; \
   b = a > 0; \
   t = b ? 20 : 40; \
   t = t OP 11; \
   return t; } \
 __GIMPLE int NAME##_test2 (int a) \
 { _Bool b; \
   int t; \
   b = a > 0; \
   t = b ? 2 : 4; \
   t = 11 OP t; \
   return t; }

F (+, plus)
F (-, minus)
F (*, mult)
F (|, bit_ior)
F (^, bit_xor)
F (/, div)
F (%, mod)
F (<<, lshift)

#define F2(OP,NAME) \
__GIMPLE int NAME##_test (int a) \
 { _Bool b; \
   int t; \
   b = a > 0; \
   t = b ? 20 : 40; \
   t = t OP 2; \
   return t; } \
__GIMPLE int NAME##_test2 (int a) \
 { _Bool b; \
   int t; \
   b = a > 0; \
   t = b ? 2 : 3; \
   t = 11 OP 2; \
   return t; }
F2 (__ROTATE_RIGHT, rotateright)
F2 (__ROTATE_LEFT, rotateleft)

__GIMPLE int test_and (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 1 : 3;
  t = t & 3;
  return t;
}

__GIMPLE int test_and2 (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 1 : 3;
  t = 3 & t;
  return t;
}

__GIMPLE int test_rshift (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 2 : 8;
  t = t >> 1;
  return t;
}

__GIMPLE int test_rshift2 (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 1 : 2;
  t = 8 >> t;
  return t;
}

static int min (int a, int b)
{
  return a < b ? a : b;
}

static int max (int a, int b)
{
  return a > b ? a : b;
}

__GIMPLE int min_test (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 2 : 4;
  t = min (t, 3);
  return t;
}

__GIMPLE int max_test (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 2 : 4;
  t = max (t, 3);
  return t;
}

char *ptr[12];
typedef __PTRDIFF_TYPE__ ptrdiff_t;

__GIMPLE ptrdiff_t pointer_diff_test (int a)
{
  _Bool b;
  ptrdiff_t t;
  char *t1;
  b = a > 0;
  t1 = b ? _Literal(char *) & ptr[0] : _Literal(char *) & ptr[1];
  t = t1 - _Literal(char *) & ptr[0];
  return t;
}

__GIMPLE ptrdiff_t pointer_diff_test2 (int a)
{
  _Bool b;
  ptrdiff_t t;
  char *t1;
  b = a > 0;
  t1 = b ? _Literal(char *) & ptr[0] : _Literal(char *) & ptr[1];
  t = _Literal(char *) & ptr[1] - t1;
  return t;
}

#if __INT_WIDTH__ == 8
#define HIGH1 0x7f
#define HIGH2 0x5f
#elif __INT_WIDTH__ == 16
#define HIGH1 0x7fff
#define HIGH2 0x5fff
#else
#define HIGH1 0x7fffffff
#define HIGH2 0x5fffffff
#endif

__GIMPLE int multhighpart_test (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? 30 : 40;
  t = t __MULT_HIGHPART HIGH1;
  return t;
}

__GIMPLE int multhighpart_test2 (int a)
{
  _Bool b;
  int t;
  b = a > 0;
  t = b ? HIGH2 : HIGH1;
  t = 40 __MULT_HIGHPART t;
  return t;
}

/* { dg-final { scan-tree-dump-times " \\+ " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\- " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\* " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\| " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\^ " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " / " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >> " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " r>> " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " h\\* " 0 "optimized" } } */

/* Note: pointer_diff_tests adds a lshift each when they succeed */
/* { dg-final { scan-tree-dump-times " << " 2 "optimized" } } */
