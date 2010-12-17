// { dg-lto-do assemble }
// { dg-extra-ld-options "-O2 -fipa-cp-clone -flto -nostdlib -r" }
#include "pr45621.h"

void
foo ()
{
  s.v1 ();
  s.m ();
}
