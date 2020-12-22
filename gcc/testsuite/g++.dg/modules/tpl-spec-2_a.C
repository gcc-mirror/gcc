// { dg-additional-options {-fmodules-ts} }

export module TPL;
// { dg-module-cmi TPL }

export template <typename T> int foo (T x) 
{
  return int (x);
}

// { dg-final { scan-assembler-not {^[a-zA-Z0-9_]*:} } }
