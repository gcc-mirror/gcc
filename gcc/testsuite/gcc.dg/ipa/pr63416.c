/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
#define _UNUSED_ __attribute__((__unused__))

typedef int TEST_F30 (int *v);
typedef void TEST_F31 (int *v);
typedef void TEST_F32 (int *v, int r);

typedef struct TEST_T30_ {
   TEST_F30 * pf30;
   TEST_F31 * pf31;
   TEST_F32 * pf32;
} TEST_T30;

static inline
int test_f30 (int *v)
{
   *v = 1;
   return 0;
}//test_f30()

static inline
void test_f31 (int *v _UNUSED_)
{
}//test_f31()

static inline
void test_f32 (int *v, int r _UNUSED_)
{
   *v = 0;
}//test_f32()

static const
TEST_T30 test_t30 = {
   .pf30 = test_f30,
   .pf31 = test_f31,
   .pf32 = test_f32,
};

static inline
int test_f10 (const TEST_T30 *pt30, int *v)
{
   int r = pt30->pf30(v);
   pt30->pf31(v);
   pt30->pf32(v, r);
   return 0;
}//test_f10()

int test_f00 (int *v)
{
   return test_f10(&test_t30, v);
}//test_f00()

/* Everything should be inlined and only test_f00 body should appear.  */
/* { dg-final { scan-tree-dump-not "test_f10" "optimized"  } } */
/* { dg-final { scan-tree-dump-not "test_f3" "optimized"  } } */
