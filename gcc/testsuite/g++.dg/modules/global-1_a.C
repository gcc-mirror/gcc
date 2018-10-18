// { dg-additional-options "-fmodules-ts" }
module;

int bar ();

export module thing;
// { dg-module-bmi "thing" }

export int baz ();
