// PR c++/114868
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;
export import :a;
