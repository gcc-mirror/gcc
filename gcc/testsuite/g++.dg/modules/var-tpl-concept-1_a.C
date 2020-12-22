// { dg-additional-options {-fmodules-ts -fconcepts} }
module;
#include "var-tpl-concept-1.h"

export module foo:part1;
// { dg-module-cmi {foo:part1} }

export void bar (string &);
