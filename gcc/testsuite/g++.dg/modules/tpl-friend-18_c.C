// PR c++/119863
// { dg-additional-options "-fmodules" }

import A;
import B;

int main()
{
  auto const x = f(1);
}
