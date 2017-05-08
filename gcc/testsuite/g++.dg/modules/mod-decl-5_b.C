

import thing.baz;

export int foo (); // { dg-error "after an interface" }

export module thing.baz; // { dg-error "already imported" }

import thing.baz;

