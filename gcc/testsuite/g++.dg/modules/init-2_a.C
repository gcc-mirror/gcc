// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Foo;
// { dg-module-cmi Foo }

static int init ()
{
  return 1;
}

int var = init ();

// { dg-final { scan-assembler {_ZGIW3Foo:} } }
