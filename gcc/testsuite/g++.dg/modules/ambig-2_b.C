// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !B }

export module B;
import A;

extern "C++" int foo ();
extern "C++" int bar ();  // { dg-error "ambiguating new declaration" }
