// PR target/65105
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-options "-O2 -march=slm" }

struct s {
  long long l1, l2, l3, l4, l5;
} *a;
long long b;
long long fn1()
{
  try
    {
      b = (a->l1 | a->l2 | a->l3 | a->l4 | a->l5);
      return a->l1;
    }
  catch (int)
    {
    }
}
