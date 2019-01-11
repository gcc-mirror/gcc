// { dg-additional-options -fmodules-ts }

export module foo:inter2; // { dg-warning "not writing" }
// { dg-module-bmi !foo:inter2 }

import :imp; // ok

import :inter; // error
// { dg-regexp "In module imported at \[^\n]*part-2_c.C:8:.:\nfoo:inter: error: interface partition must be exported\n" }
