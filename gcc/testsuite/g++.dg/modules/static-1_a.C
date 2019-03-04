// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module;
# 5 "gmf" 1
static void Bar () {}
# 7 "" 2
export module Foo;
// { dg-module-bmi Foo }

static void Baz () {}

