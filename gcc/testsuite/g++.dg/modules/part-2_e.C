// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

import :imp; // ok

export import :inter2; // ok

export import :inter; // ok now

