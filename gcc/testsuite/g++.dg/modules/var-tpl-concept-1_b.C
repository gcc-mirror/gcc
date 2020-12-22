// { dg-additional-options {-fmodules-ts -fconcepts} }
module;
#include "var-tpl-concept-1.h"

export module foo;
// { dg-module-cmi {foo} }

export import :part1;

export void baz (string &);
