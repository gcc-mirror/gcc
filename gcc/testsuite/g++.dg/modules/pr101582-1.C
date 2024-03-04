// PR c++/101582
// { dg-additional-options "-fmodules-ts" }
export module pr101582;
// { dg-module-cmi "pr101582" }

// These are all legal since P2615R1.
export ;
export [[]];
export
{
}
