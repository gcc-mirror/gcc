struct c {
  double a;
} __attribute((packed)) __attribute((aligned));

extern void abort(void);

double g_expect = 32.25;

void f(unsigned x, struct c y)
{
  if (x != 0)
    abort();

  if (y.a != g_expect)
    abort();
}

struct c e = { 64.25 };

int main(void)
{
  struct c d = { 32.25 };
  f(0, d);

  g_expect = 64.25;
  f(0, e);
  return 0;
}
