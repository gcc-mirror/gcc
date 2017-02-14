/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-cp" } */

__attribute__((noinline))
static int f(int x)
{
  extern int limit;
  extern int f2(int);

  if (x == limit)
    return x;
  int k = f(x + 1);
  return f2 (k); 
}

int main(int argc, char **argv)
{
  int k = f(argc & 0xff); 
  return k;
}

/* { dg-final { scan-ipa-dump-not "Adjusting mask for" "cp" } } */  
