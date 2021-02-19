// { dg-additional-options {-fmodules-ts -flang-info-module-read=Foo} }
module Foo;

// { dg-regexp {In module imported at [^\n]*pr99166_c.C:[0-9:]*\n[^\b]*Foo: note: reading CMI 'gcm.cache/Foo.gcm'\n} }
