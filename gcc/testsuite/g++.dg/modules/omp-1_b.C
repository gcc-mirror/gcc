// { dg-additional-options "-fmodules-ts -fopenmp" }

import foo;

unsigned ary[64];

int main ()
{
  frob (ary);
}
