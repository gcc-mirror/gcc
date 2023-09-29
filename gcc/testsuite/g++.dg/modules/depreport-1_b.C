// { dg-additional-options -fmodules-ts }
// { dg-additional-options -MD }
// { dg-additional-options "-MF depreport-1.d" }

import Foo;

void foo ()
{
  Base b;
}

// { dg-final { run-check-module-dep-expect-input "depreport-1.d" "gcm.cache/Foo.gcm" } }
