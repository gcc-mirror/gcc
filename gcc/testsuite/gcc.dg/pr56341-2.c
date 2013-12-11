/* { dg-do run } */
/* { dg-options "-fno-strict-volatile-bitfields" } */

extern void abort (void);

struct test0
{
  unsigned char b1[2];
} __attribute__((packed, aligned(2)));

struct test1
{
  volatile unsigned long a1;
  unsigned char b1[4];
} __attribute__((packed, aligned(2)));

struct test2
{
  struct test0 t0;
  struct test1 t1;
  struct test0 t2;
} __attribute__((packed, aligned(2)));

struct test2 xx;
struct test2 *x1 = &xx;

#define MAGIC 0x12345678

void test0 (struct test2* x1)
{
  x1->t1.a1 = MAGIC;
}

int main()
{
  test0 (x1);
  if (xx.t1.a1 != MAGIC)
    abort ();
  return 0;
}
