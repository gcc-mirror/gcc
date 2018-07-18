// { dg-do run }

void *operator new(__SIZE_TYPE__, void *p2) { return p2; }
struct B { B(int i_) : i(i_) {} int i; };
struct X
{
  unsigned char buf[sizeof (B)];
};

int __attribute__((noinline)) foo()
{
  X x alignas(B), y alignas(B);
  new (&x) B (0);
  y = x;
  B *q = reinterpret_cast <B *>(&y);
  asm volatile ("" : "=r" (q) : "0" (q));
  return q->i;
}

int main()
{
  if (foo() != 0)
    __builtin_abort ();
  return 0;
}
