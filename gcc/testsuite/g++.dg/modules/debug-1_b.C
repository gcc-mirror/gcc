// { dg-additional-options "-fmodules-ts -g" }

import frob;

struct thongy : thingy
{
  void X ()
  {
    thongy w;
  }
};
