typedef int __v1si __attribute__ ((__vector_size__ (4)));
typedef int __v2si __attribute__ ((__vector_size__ (8)));
typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));
typedef unsigned char __v4qi __attribute__ ((__vector_size__ (4)));
typedef unsigned char __v8qi __attribute__ ((__vector_size__ (8)));

extern void abort (void);

static void
compare64 (void *p, unsigned long long val)
{
  if (*(unsigned long long *)p != val)
    abort();
}

static void
compare32 (void *p, unsigned int val)
{
  if (*(unsigned int *)p != val)
    abort();
}

static void
test_v8qi (unsigned char x)
{
  __v8qi v = { x, x, x, x, x, x, x, x };

  compare64(&v, 0x4444444444444444ULL);
}

static void
test_v4qi (unsigned char x)
{
  __v4qi v = { x, x, x, x };

  compare32(&v, 0x44444444);
}

static void
test_v4hi (unsigned short x)
{
  __v4hi v = { x, x, x, x, };

  compare64(&v, 0x3344334433443344ULL);
}

static void
test_v2hi (unsigned short x)
{
  __v2hi v = { x, x, };

  compare32(&v, 0x33443344);
}

static void
test_v2si (unsigned int x)
{
  __v2si v = { x, x, };

  compare64(&v, 0x1122334411223344ULL);
}

static void
test_v1si (unsigned int x)
{
  __v1si v = { x };

  compare32(&v, 0x11223344);
}

unsigned char x8 = 0x44;
unsigned short x16 = 0x3344;
unsigned int x32 = 0x11223344;

int main(void)
{
  test_v8qi (x8);
  test_v4qi (x8);
  test_v4hi (x16);
  test_v2hi (x16);
  test_v2si (x32);
  test_v1si (x32);
  return 0;
}
