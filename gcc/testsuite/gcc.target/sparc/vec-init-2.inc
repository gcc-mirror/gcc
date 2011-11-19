typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));

extern void abort (void);

static void
compare64 (int n, void *p, unsigned long long val)
{
  unsigned long long *x = (unsigned long long *) p;

  if (*x != val)
    abort();
}

static void
compare32 (int n, void *p, unsigned int val)
{
  unsigned int *x = (unsigned int *) p;
  if (*x != val)
    abort();
}

#define V2HI_TEST(N, elt0, elt1) \
static void \
test_v2hi_##N (unsigned short x, unsigned short y) \
{ \
  __v2hi v = { (elt0), (elt1) }; \
  compare32(N, &v, ((int)(elt0) << 16) | (elt1)); \
}

V2HI_TEST(1, x, y)
V2HI_TEST(2, y, x)
V2HI_TEST(3, x, x)
V2HI_TEST(4, x, 0)
V2HI_TEST(5, 0, x)
V2HI_TEST(6, y, 1)
V2HI_TEST(7, 1, y)
V2HI_TEST(8, 2, 3)
V2HI_TEST(9, 0x400, x)
V2HI_TEST(10, y, 0x8000)

#define V4HI_TEST(N, elt0, elt1, elt2, elt3)	\
static void \
test_v4hi_##N (unsigned short a, unsigned short b, unsigned short c, unsigned short d) \
{ \
  __v4hi v = { (elt0), (elt1), (elt2), (elt3) }; \
  compare64(N, &v, \
            ((long long)(elt0) << 48) | \
	    ((long long)(elt1) << 32) | \
            ((long long)(elt2) << 16) | \
            ((long long)(elt3))); \
}

V4HI_TEST(1, a, a, a, a)
V4HI_TEST(2, a, b, c, d)
V4HI_TEST(3, a, a, b, b)
V4HI_TEST(4, d, c, b, a)
V4HI_TEST(5, a, 0, 0, 0)
V4HI_TEST(6, a, 0, b, 0)
V4HI_TEST(7, c, 5, 5, 5)
V4HI_TEST(8, d, 6, a, 6)
V4HI_TEST(9, 0x200, 0x300, 0x500, 0x8800)
V4HI_TEST(10, 0x600, a, a, a)

unsigned short a16 = 0x3344;
unsigned short b16 = 0x5566;
unsigned short c16 = 0x7788;
unsigned short d16 = 0x9911;

int main(void)
{
  test_v2hi_1 (a16, b16);
  test_v2hi_2 (a16, b16);
  test_v2hi_3 (a16, b16);
  test_v2hi_4 (a16, b16);
  test_v2hi_5 (a16, b16);
  test_v2hi_6 (a16, b16);
  test_v2hi_7 (a16, b16);
  test_v2hi_8 (a16, b16);
  test_v2hi_9 (a16, b16);
  test_v2hi_10 (a16, b16);

  test_v4hi_1 (a16, b16, c16, d16);
  test_v4hi_2 (a16, b16, c16, d16);
  test_v4hi_3 (a16, b16, c16, d16);
  test_v4hi_4 (a16, b16, c16, d16);
  test_v4hi_5 (a16, b16, c16, d16);
  test_v4hi_6 (a16, b16, c16, d16);
  test_v4hi_7 (a16, b16, c16, d16);
  test_v4hi_8 (a16, b16, c16, d16);
  test_v4hi_9 (a16, b16, c16, d16);
  test_v4hi_10 (a16, b16, c16, d16);
  return 0;
}
