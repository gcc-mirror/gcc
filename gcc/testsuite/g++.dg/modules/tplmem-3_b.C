// { dg-additional-options "-fmodules-ts" }

import billy.bob.thornton;

int main ()
{
  Outer<4>::Inner<16> v;

  if (v.m () != 64)
    return 1;
  return 0;
}

