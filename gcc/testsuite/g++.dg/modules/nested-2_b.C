// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

import bob;

int main ()
{
  X ary[10];
  X::iter iter;
  unsigned ix;

  for (ix = 10, iter = ary; ix--; iter++)
    iter->set (ix);

  for (ix = 10; ix--;)
    if (ary[ix] + ix != 9)
      return 1;
  
  return 0;
}
