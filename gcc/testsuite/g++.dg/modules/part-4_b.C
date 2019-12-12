// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// detecting an ICE in dumping machinery

export module foo;
// { dg-module-cmi foo }

export import :part1;

export frob foo ()
{
  return frob ();
}
