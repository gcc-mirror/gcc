typedef unsigned char __v4qi __attribute__ ((__vector_size__ (4)));
typedef unsigned char __v8qi __attribute__ ((__vector_size__ (8)));

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

#define V4QI_TEST(N, elt0, elt1, elt2, elt3)	\
static void \
test_v4qi_##N (unsigned char a, unsigned char b, unsigned char c, unsigned char d) \
{ \
  __v4qi v = { (elt0), (elt1), (elt2), (elt3) };	\
  compare32(N, &v, ((int)(elt0) << 24) | \
	           ((int)(elt1) << 16) | \
	           ((int)(elt2) << 8) | ((int)(elt3)));	\
}

V4QI_TEST(1, a, a, a, a)
V4QI_TEST(2, b, b, b, b)
V4QI_TEST(3, a, b, c, d)
V4QI_TEST(4, d, c, b, a)
V4QI_TEST(5, a, 0, 0, 0)
V4QI_TEST(6, b, 1, 1, b)
V4QI_TEST(7, c, 5, d, 5)
V4QI_TEST(8, 0x20, 0x30, b, a)
V4QI_TEST(9, 0x40, 0x50, 0x60, 0x70)
V4QI_TEST(10, 0x40, 0x50, 0x60, c)

#define V8QI_TEST(N, elt0, elt1, elt2, elt3, elt4, elt5, elt6, elt7) \
static void \
test_v8qi_##N (unsigned char a, unsigned char b, unsigned char c, unsigned char d, \
               unsigned char e, unsigned char f, unsigned char g, unsigned char h) \
{ \
  __v8qi v = { (elt0), (elt1), (elt2), (elt3), \
	       (elt4), (elt5), (elt6), (elt7) }; \
  compare64(N, &v, ((long long)(elt0) << 56) | \
	           ((long long)(elt1) << 48) | \
	           ((long long)(elt2) << 40) | \
	           ((long long)(elt3) << 32) | \
	           ((long long)(elt4) << 24) | \
	           ((long long)(elt5) << 16) | \
	           ((long long)(elt6) << 8) | \
	           ((long long)(elt7) << 0)); \
}

V8QI_TEST(1, a, a, a, a, a, a, a, a)
V8QI_TEST(2, a, b, c, d, e, f, g, h)
V8QI_TEST(3, h, g, f, e, d, c, b, a)
V8QI_TEST(4, a, b, a, b, a, b, a, b)
V8QI_TEST(5, c, b, c, b, c, b, c, a)
V8QI_TEST(6, a, 0, 0, 0, 0, 0, 0, 0)
V8QI_TEST(7, b, 1, b, 1, b, 1, b, 1)
V8QI_TEST(8, c, d, 0x20, a, 0x21, b, 0x23, c)
V8QI_TEST(9, 1, 2, 3, 4, 5, 6, 7, 8)
V8QI_TEST(10, a, a, b, b, c, c, d, d)

unsigned char a8 = 0x33;
unsigned char b8 = 0x55;
unsigned char c8 = 0x77;
unsigned char d8 = 0x99;
unsigned char e8 = 0x11;
unsigned char f8 = 0x22;
unsigned char g8 = 0x44;
unsigned char h8 = 0x66;

int main(void)
{
  test_v4qi_1 (a8, b8, c8, d8);
  test_v4qi_2 (a8, b8, c8, d8);
  test_v4qi_3 (a8, b8, c8, d8);
  test_v4qi_4 (a8, b8, c8, d8);
  test_v4qi_5 (a8, b8, c8, d8);
  test_v4qi_6 (a8, b8, c8, d8);
  test_v4qi_7 (a8, b8, c8, d8);
  test_v4qi_8 (a8, b8, c8, d8);
  test_v4qi_9 (a8, b8, c8, d8);
  test_v4qi_10 (a8, b8, c8, d8);

  test_v8qi_1 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_2 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_3 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_4 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_5 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_6 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_7 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_8 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_9 (a8, b8, c8, d8, e8, f8, g8, h8);
  test_v8qi_10 (a8, b8, c8, d8, e8, f8, g8, h8);
  return 0;
}
