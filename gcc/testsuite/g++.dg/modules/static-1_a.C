// { dg-additional-options "-fmodules-ts" }
module;
static void Bar () {}

export module Foo;
// { dg-module-bmi Foo }

static void Baz () {}

