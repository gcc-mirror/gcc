// PR c++/123627
// { dg-additional-options "-fmodules" }
// { dg-module-cmi m }

export module m;
export import :part;
