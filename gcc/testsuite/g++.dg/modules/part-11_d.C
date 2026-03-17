// PR c++/124309
// { dg-additional-options "-fmodules" }
// { dg-module-cmi Z }

export module Z;
export import Y;
export import :part;
