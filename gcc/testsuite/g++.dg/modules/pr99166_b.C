// { dg-additional-options {-fmodules-ts -flang-info-module-cmi=<iostream>} }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

export module Foo;
import <iostream>;

export void frob ();

// { dg-regexp {In module imported at [^\n]*pr99166_b.C:[0-9:]*\n[^\n]*iostream: note: reading CMI 'gcm.cache/[^[\n]*iostream.gcm'\n} }
