// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

export using aint = __attribute__ ((aligned(16))) int;


