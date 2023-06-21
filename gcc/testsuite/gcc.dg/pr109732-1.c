/* { dg-do run } */
/* fre1 is disable because it can figure out that the if return value is just `a` */
/* { dg-options "-O1 -fdisable-tree-fre1" } */

/* This code is done this way to have the false edge as 1st
   successor edge of BB2. Normally the true edge would be
   the first and you would not hit the bug. Also the way PHIs is
   done needs to be such that 0 is first. */
[[gnu::noipa]]
_Bool f3(_Bool a)
{
        if (a==0)
          return 0;
        else
          return a;
}

int main()
{
  if (f3(0))
    __builtin_abort();
  if (!f3(1))
    __builtin_abort();
}
