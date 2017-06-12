// { dg-lto-do run }

void *operator new(__SIZE_TYPE__, void *p2) { return p2; }
struct B { B(int i_) : i(i_) {} int i; };
struct X
{
  unsigned char buf[sizeof (B)];
};

int __attribute__((noinline)) foo()
{
  X x alignas (B), y alignas (B);
  new (&x) B (0);
  y = x;
  B *q = reinterpret_cast <B *>(&y);
  asm volatile ("" : "+r" (q));
  return q->i;
}
extern "C" void bar ();
int main()
{
  if (foo() != 0)
    __builtin_abort ();
  bar ();
  return 0;
}
