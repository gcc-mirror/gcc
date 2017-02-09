

import thing.baz;

export int foo (); // { dg-error "after an interface" }

module thing.baz [[interface]]; // { dg-error "already imported" }
// { dg-module-if "!thing.baz" }

import thing.baz;

