// { dg-additional-options "-fmodules -Wmaybe-uninitialized" }

import M;

int main()
{
  A a;
  f(a);
}
