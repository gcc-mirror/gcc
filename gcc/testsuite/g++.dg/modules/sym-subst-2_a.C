// { dg-additional-options "-fmodules-ts" }

export module bob.stuart;
// { dg-module-cmi bob.stuart }

template <typename T> void inner (T &)
{
}

export template <typename T> void foo (T &x)
{
  inner (x);
}

// { dg-final { scan-assembler-not {all must have scan-assembler} } }
