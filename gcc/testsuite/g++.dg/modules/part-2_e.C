// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi foo }

import :imp; // ok

export import :inter2; // ok

export import :inter; // ok now

