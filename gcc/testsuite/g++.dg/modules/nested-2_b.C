// { dg-module-do run }

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
