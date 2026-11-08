/* { dg-do run } */
/* { dg-options "-O2 -fgimple -fdump-tree-ifcombine-details" } */

/* Check that a widened signed field compared with another one is still
   combined when a mask zeroes out all of its extension bits.  */

struct S { signed char a, b; short c; } __attribute__ ((aligned (4)));
struct S p, q;

int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f1 ()
{
  signed char _1;
  signed char _2;
  int _3;
  int _4;
  int _5;
  int _6;
  signed char _7;
  signed char _8;
  int r;

  __BB(2):
  _1 = p.a;
  _3 = (int) _1;
  _5 = _3 & 127;
  _2 = q.a;
  _4 = (int) _2;
  _6 = _4 & 127;
  if (_5 == _6)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  _7 = p.b;
  _8 = q.b;
  if (_7 == _8)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_9 = __PHI (__BB3: 1, __BB4: 0);
  return r_9;
}

int __GIMPLE (ssa,startwith("ifcombine")) __attribute__((noipa))
f2 ()
{
  signed char _1;
  signed char _2;
  int _3;
  int _4;
  int _5;
  int _6;
  signed char _7;
  signed char _8;
  int r;

  __BB(2):
  _1 = p.a;
  _3 = (int) _1;
  _5 = _3 & 255;
  _2 = q.a;
  _4 = (int) _2;
  _6 = _4 & 255;
  if (_5 == _6)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  _7 = p.b;
  _8 = q.b;
  if (_7 == _8)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  goto __BB5;

  __BB(5):
  r_9 = __PHI (__BB3: 1, __BB4: 0);
  return r_9;
}

int
main ()
{
  p.a = -1;
  q.a = 127;
  p.b = q.b = -2;
  if (f1 () != 1 || f2 () != 0)
    __builtin_abort ();
  q.b = 2;
  if (f1 () != 0 || f2 () != 0)
    __builtin_abort ();
  q.a = -1;
  q.b = -2;
  if (f1 () != 1 || f2 () != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing two comparisons" 2 "ifcombine" } } */
