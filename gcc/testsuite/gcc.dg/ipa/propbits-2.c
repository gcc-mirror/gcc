/* x's mask should be meet(0xc, 0x3) == 0xf  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-cp" } */

extern int pass_test ();
extern int fail_test ();

__attribute__((noinline))
static int f1(int x)
{
  if ((x & ~0xf) == 0)
    return pass_test ();
  else
    return fail_test ();
}

__attribute__((noinline))
static int f2(int y)
{
  return f1(y & 0x03);
}

__attribute__((noinline))
static int f3(int z)
{
  return f1(z & 0xc);
}

extern int a;
extern int b;

int main(void)
{
  int k = f2(a); 
  int l = f3(b);
  return k + l;
}

/* { dg-final { scan-ipa-dump "Adjusting mask for param 0 to 0xf" "cp" } } */
/* { dg-final { scan-dump-tree-not "fail_test" "optimized" } } */
