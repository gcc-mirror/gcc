// PR c++/114947
// { dg-additional-options "-fmodules-ts -std=c++20" }
// { dg-module-cmi M }
export module M;
import :part;
