// PR c++/124311
// { dg-additional-options "-fmodules" }
// { dg-module-cmi B }

export module B;
export import A;
export struct M { S h; };
M m;
