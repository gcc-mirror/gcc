// REQUIRED_ARGS: -verrors=simple
// PERMUTE_ARGS:
// EXTRA_FILES: imports/udamodule1.d
/*
TEST_OUTPUT:
---
compilable/udamodule1.d(10): Deprecation: module `imports.udamodule1` is deprecated - This module will be removed.
---
*/
import imports.udamodule1;

void main() { foo(); }
