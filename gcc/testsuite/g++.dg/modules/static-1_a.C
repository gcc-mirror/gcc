// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;
# 5 "gmf" 1
static void Bar () {}
# 7 "" 2
export module Foo;
// { dg-module-cmi Foo }

static void Baz () {}

