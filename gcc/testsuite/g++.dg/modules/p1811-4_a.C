// Definitions attached to a named module cannot be repeated.

// { dg-additional-options "-fmodules" }
// { dg-module-cmi P:a }

export module P:a;

export struct A { };
