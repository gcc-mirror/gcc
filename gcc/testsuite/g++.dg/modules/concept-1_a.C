// { dg-additional-options "-fmodules-ts -fconcepts" }

export module foo;
// { dg-module-cmi foo }

export template<typename T>
requires (!!sizeof (bool (T{})))
T f1 (T x)
{ return x; }

