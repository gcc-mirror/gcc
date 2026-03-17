// PR c++/124309
// { dg-additional-options "-fmodules" }
// { dg-module-cmi Z:part }

export module Z:part;
export import X;
