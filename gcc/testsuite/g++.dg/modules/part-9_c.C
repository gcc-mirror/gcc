// PR c++/114950
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }
// Handle merging definitions of extern "C++" decls across partitions

export module M;
import :a;
import :b;
