// { dg-do compile }
// { dg-options "-O2 -fdump-tree-fre1-details" }

struct a
{
  int a;
  static __attribute__ ((noinline))
      int ret (int v) {return v;}

  __attribute__ ((noinline))
      int inca () {return a++;}
};

int
test()
{
  struct a av;
  av.a=1;
  int val = av.ret (0) + av.inca();
  av.a=2;
  return val + av.ret(0) + av.inca();
}

/* { dg-final { scan-tree-dump-times "Replaced a::ret" 1 "fre1" } } */
