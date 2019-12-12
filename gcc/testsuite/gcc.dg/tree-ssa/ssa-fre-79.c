/* { dg-do run } */
/* { dg-options "-O -fgimple -fdump-tree-fre1" } */

struct S { char a[4]; };
const struct S cs = { 1, 2, 3, 4 };

int __GIMPLE(ssa,startwith("fre"))
main ()
{
  struct S s;
  short _1;

  __BB(2):
  s = cs;
  s.a[1] = _Literal (char) 3;
  _1 = __MEM <short, 1> (&s + 1);
  if (_1 != _Literal (short) 0x303)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  __builtin_abort ();

  __BB(4):
  return 0;
}

/* { dg-final { scan-tree-dump-not "abort" "fre1" } } */
