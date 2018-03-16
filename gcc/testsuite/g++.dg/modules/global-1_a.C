// { dg-additional-options "-fmodules-ts" }
// { dg-module-bmi "thing" }

module;

int bar ();

export module thing;

export int baz ();
