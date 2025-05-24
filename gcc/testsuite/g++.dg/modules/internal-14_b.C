// PR c++/120412
// { dg-additional-options "-fmodules -std=c++20 -Wtemplate-names-tu-local" }
// { dg-module-cmi m }

export module m;
export import :part;
