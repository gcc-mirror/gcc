// { dg-additional-options "-fmodules-ts -fopenacc" }
// { dg-require-effective-target pthread }

import foo;

unsigned ary[64];

int main ()
{
  frob (ary);
}
