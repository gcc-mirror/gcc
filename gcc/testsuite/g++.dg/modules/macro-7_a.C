// { dg-additional-options -fmodules-ts }
export module foo;
// { dg-module-cmi foo }

#define MACRO(X) X

export template<int I> int Factory ()
{
  // this macro expansion location ends up in the instantiation
  // emitted by an importer
  return MACRO(I);
}

