
void abort (void);

struct T
{
  int b : 1;
} t;

void __attribute__((noinline)) foo (int f)
{
  t.b = (f & 0x10) ? 1 : 0;
}

int main (void)
{
  foo (0x10);
  if (!t.b)
    abort ();
  return 0;
}
