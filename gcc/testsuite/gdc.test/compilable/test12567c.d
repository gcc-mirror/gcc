// REQUIRED_ARGS:
// EXTRA_FILES: imports/a12567.d
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
compilable/test12567c.d(10): Deprecation: module `imports.a12567` is deprecated - This module will be removed in future release.
---
*/
import imports.a12567;

void main() { foo(); }
