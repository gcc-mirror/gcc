// { dg-lto-do assemble }
// { dg-extra-ld-options "-O2 -fipa-cp-clone -flto -nostdlib -r -flinker-output=nolto-rel" }
#include "pr45621.h"

void
foo ()
{
  s.v1 ();
  s.m ();
}
