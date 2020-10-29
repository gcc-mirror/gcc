// { dg-additional-options {-fmodules-ts -fno-module-lazy -fdump-lang-module-alias} }

import "enum-8_b.H";

import Char;


// { dg-final { scan-lang-dump {Read:-1's enum merge key \(new\) type_decl:'#null#'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) const_decl:'::._anon_0@[^\n]*/enum-8_b.H:1::Foo'} module } }
// { dg-final { scan-lang-dump {Read:-1's enum merge key \(new\) type_decl:'#null#'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) const_decl:'::._anon_1@[^\n]*/enum-8_a.H:2::Foo'} module } }
