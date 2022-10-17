/*
EXTRA_FILES: imports/imp20709.d
TEST_OUTPUT:
---
fail_compilation/ice20709.d(10): Error: module `imp20709` import `Point` not found
---
*/
module ice20709;

import imports.imp20709 : Point;

immutable Point aPoint = somePoint;

Point somePoint() {}
