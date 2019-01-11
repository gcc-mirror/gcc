// { dg-additional-options -fmodules-ts }

export module foo; // { dg-warning "not writing" }
// { dg-module-bmi !foo }

import :imp; // error
// { dg-regexp "In module imported at \[^\n]*part-2_d.C:6:.:\nfoo:imp: error: interface partition ':inter' has not been exported\n" }

import :inter; // error
// { dg-regexp "In module imported at \[^\n]*part-2_d.C:9:.:\nfoo:inter: error: interface partition must be exported\n" }

