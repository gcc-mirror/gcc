// { dg-additional-options "-fmodules-ts" }
module;

import thing.baz;

export int foo (); // { dg-error "after a module interface" }

export module thing.baz; // { dg-error "cannot declare module after import" }

import thing.baz;

