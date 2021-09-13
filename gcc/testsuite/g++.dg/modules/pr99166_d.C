// { dg-additional-options {-fmodules-ts -flang-info-module-cmi} }
import Foo;

// { dg-regexp {In module imported at [^\n]*pr99166_d.C:[0-9:]*\n[^\n]*Foo: note: reading CMI 'gcm.cache/Foo.gcm'\n} }

// { dg-regexp {In module imported at [^\n]*pr99166_b.C:[0-9:]*,\nof module Foo, imported at [^\n]*pr99166_d.C:[0-9:]:\n[^\n]*iostream: note: reading CMI 'gcm.cache/[^[\n]*iostream.gcm'\n} }
