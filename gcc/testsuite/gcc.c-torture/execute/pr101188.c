/* { dg-require-effective-target indirect_calls } */
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;

typedef uint8_t (*fn1)(void *a);
typedef void (*fn2)(void *a, int *arg);

struct S
{
    uint8_t buffer[64];
    uint16_t n;
    fn2 f2;
    void *a;
    fn1 f1;
};

volatile uint16_t x;

void __attribute__((__noinline__,__noclone__))
foo (uint16_t n)
{
  x = n;
}

void __attribute__((__noinline__,__noclone__))
testfn (struct S *self)
{
    int arg;

    foo (self->n);
    self->n++;
    self->f2 (self->a, &arg);
    self->buffer[0] = self->f1 (self->a);
}

static unsigned char myfn2_called = 0;

static void
myfn2 (void *a, int *arg)
{
  myfn2_called = 1;
}

static uint8_t
myfn1 (void *a)
{
  return 0;
}

int main (void)
{
  struct S s;
  s.n = 0;
  s.f2 = myfn2;
  s.f1 = myfn1;
  testfn (&s);
  if (myfn2_called != 1)
    __builtin_abort();
  return 0;
}

