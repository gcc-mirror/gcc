// PR c++/124309
// { dg-additional-options "-fmodules" }
// { dg-module-cmi X }

export module X;

export void frob(int*);
