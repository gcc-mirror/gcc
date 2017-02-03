

import thing.baz;

export int foo (); // { dg-error "after an interface" }

module thing.baz [[interface]]; // { dg-error "already imported" }

import thing.baz;

