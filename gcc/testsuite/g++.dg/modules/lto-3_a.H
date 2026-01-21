// PR c++/118961
// { dg-additional-options "-fmodule-header -std=c++20" }
// { dg-module-cmi {} }
// { dg-skip-if "required hosted libstdc++ for string" { ! hostedlib } }
// We shouldn't ICE when linking against the standard library.

#include <string>
