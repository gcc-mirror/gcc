/* Copyright (C) 2003  Free Software Foundation.
   PR target/13256
   STRICT_LOW_PART was handled incorrectly in delay slots.
   Origin: Hans-Peter Nilsson.  */

typedef struct { unsigned int e0 : 16; unsigned int e1 : 16; } s1;
typedef struct { unsigned int e0 : 16; unsigned int e1 : 16; } s2;
typedef struct { s1 i12; s2 i16; } io;
static int test_length = 2;
static io *i;
static int m = 1;
static int d = 1;
static unsigned long test_t0;
static unsigned long test_t1;
void test(void) __attribute__ ((__noinline__));
extern int f1 (void *port) __attribute__ ((__noinline__));
extern void f0 (void) __attribute__ ((__noinline__));
int
f1 (void *port)
{
  int fail_count = 0;
  unsigned long tlen;
  s1 x0 = {0};
  s2 x1 = {0};

  i = port;
  x0.e0 = x1.e0 = 32;
  i->i12 = x0;
  i->i16 = x1;
  do f0(); while (test_t1);
  x0.e0 = x1.e0 = 8;
  i->i12 = x0;
  i->i16 = x1;
  test ();
  if (m)
    {
      unsigned long e = 1000000000 / 460800 * test_length;
      tlen = test_t1 - test_t0;
      if (((tlen-e) & 0x7FFFFFFF) > 1000)
	f0();
    }
  if (d)
    {
      unsigned long e = 1000000000 / 460800 * test_length;
      tlen = test_t1 - test_t0;
      if (((tlen - e) & 0x7FFFFFFF) > 1000)
	f0();
    }
  return fail_count != 0 ? 1 : 0;
}

int
main ()
{
  io io0;
  f1 (&io0);
  abort ();
}

void
test (void)
{
  io *iop = i;
  if (iop->i12.e0 != 8 || iop->i16.e0 != 8)
    abort ();
  exit (0);
}

void
f0 (void)
{
  static int washere = 0;
  io *iop = i;
  if (washere++ || iop->i12.e0 != 32 || iop->i16.e0 != 32)
    abort ();
}
