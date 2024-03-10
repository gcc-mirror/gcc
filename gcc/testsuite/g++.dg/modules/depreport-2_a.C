// { dg-do preprocess }
// { dg-additional-options -fmodules-ts }
// { dg-additional-options -fmodule-mapper=[srcdir]/depreport-2.modmap }
// { dg-additional-options -MD }
// { dg-additional-options "-MF depreport-2_a.d" }

export module Foo;

export class Base
{
public:
  int m;
};

// { dg-final { run-check-module-dep-expect-input "depreport-2_a.d" "[srcdir]/depreport-2.modmap" } }
