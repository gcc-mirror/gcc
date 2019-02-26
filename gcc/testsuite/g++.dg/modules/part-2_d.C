// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi !foo }

import :imp; // ok

export import :inter2; // ok

// { dg-regexp "In module imported at \[^\n]*part-2_b.C:6:.,\nof module :imp, imported at \[^\n]*part-2_d.C:6:\n:inter: error: interface partition is not exported\n" }
