// { dg-additional-options {-fmodules-ts -flang-info-module-read=<iostream>} }
export module Foo;
import <iostream>;

export void frob ();

// { dg-regexp {In module imported at [^\n]*pr99166_b.C:[0-9:]*\n[^\b]*iostream: note: reading CMI 'gcm.cache/[^[\n]*iostream.gcm'\n} }
