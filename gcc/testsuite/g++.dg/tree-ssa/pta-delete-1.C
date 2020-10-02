// { dg-do run }
// { dg-options "-O2" }

struct X {
  static struct X saved;
  int *p;
  X() { __builtin_memcpy (this, &saved, sizeof (X)); }
};
X X::saved;
void __attribute__((noinline)) operator delete (void *p)
{
  __builtin_memcpy (&X::saved, p, sizeof (X));
}
int main()
{
  int y = 1;
  X *p = new X;
  p->p = &y;
  ::operator delete (p);
  X *q = new X;
  *(q->p) = 2;
  if (y != 2)
    __builtin_abort ();
}
