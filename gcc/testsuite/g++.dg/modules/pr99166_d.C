// { dg-additional-options {-fmodules-ts -flang-info-module-read} }
import Foo;

// { dg-regexp {In module imported at [^\n]*pr99166_d.C:[0-9:]*\n[^\b]*Foo: note: reading CMI 'gcm.cache/Foo.gcm'\n} }

// { dg-regexp {In module imported at [^\n]*pr99166_b.C:[0-9:]*,\nof module Foo, imported at [^\n]*pr99166_d.C:[0-9:]:\n[^\b]*iostream: note: reading CMI 'gcm.cache/[^[\n]*iostream.gcm'\n} }
