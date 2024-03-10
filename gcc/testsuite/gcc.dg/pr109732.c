/* { dg-do run } */
/* We need to disable passes which might cause cfg cleanup */
/* { dg-options "-O1 -fgimple -fdisable-tree-ethread -fdisable-tree-fre1" } */

/* This code is done this way to have the false edge as 1st
   successor edge of BB2. Normally the true edge would be
   the first and you would not hit the bug.  */
[[gnu::noipa]]
_Bool __GIMPLE (ssa, startwith("forwprop1"))
f3 (_Bool a)
{
  _Bool i;
  _Bool tt;

  __BB(2):
  tt_4 = a_1(D) == _Literal (_Bool)0;
  if (tt_4 != _Literal (_Bool)0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
    goto __BB5;

  __BB(4):
    goto __BB5;

  __BB(5):
  i_2 = __PHI (__BB4: a_1(D), __BB3: _Literal (_Bool)0);

  return i_2;
}

int main()
{
  if (f3(0))
    __builtin_abort();
  if (!f3(1))
    __builtin_abort();
}
