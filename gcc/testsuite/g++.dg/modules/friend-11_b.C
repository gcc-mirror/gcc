// { dg-additional-options "-fmodules" }

import M;

int main()
{
  fn<A<int>>();
  fn<B<int>>();
}
