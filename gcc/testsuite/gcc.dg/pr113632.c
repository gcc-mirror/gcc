/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void dummy(void);
_Bool f(unsigned long a)
{
        _Bool cmp = a > 8192;
        if (cmp) goto then; else goto e;
then:
        unsigned long t = __builtin_clzl(a); // [0,50]
        t^=63; // [13,63]
        if (t < 13 || t >63)
          dummy ();
e:
  return 0;
}

void f2(int x)
{
  if (x <= 0 || x == 2 || x == 4 || x == 6)
    return;
  /* x = [1, 1][3, 3][5, 5][7, 2147483647] */
  /* x ^ 6  should be non-zero.  */
  if ((x ^ 6) == 0)
    dummy ();
}

/* { dg-final { scan-tree-dump-not "dummy"  "evrp" } } */
