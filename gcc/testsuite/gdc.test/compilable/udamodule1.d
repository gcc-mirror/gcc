// REQUIRED_ARGS:
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
compilable/udamodule1.d(9): Deprecation: module imports.udamodule1 is deprecated - This module will be removed.
---
*/
import imports.udamodule1;

void main() { foo(); }
