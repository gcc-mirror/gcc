// REQUIRED_ARGS:
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
compilable/test12567c.d(9): Deprecation: module imports.a12567 is deprecated - This module will be removed in future release.
---
*/
import imports.a12567;

void main() { foo(); }
