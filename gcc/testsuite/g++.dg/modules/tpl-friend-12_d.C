// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;
export import :B;
export import :C;

export int go_in_module();
