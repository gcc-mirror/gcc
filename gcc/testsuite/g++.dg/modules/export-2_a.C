// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi aliases }

export module aliases;

typedef int x;
export typedef int x;

using y = double;
export using y = double;

struct S {};
using T = S;
export using T = S;
