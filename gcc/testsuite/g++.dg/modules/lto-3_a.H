// PR c++/118961
// { dg-additional-options "-fmodule-header -std=c++20" }
// { dg-module-cmi {} }
// We shouldn't ICE when linking against the standard library.

#include <string>
