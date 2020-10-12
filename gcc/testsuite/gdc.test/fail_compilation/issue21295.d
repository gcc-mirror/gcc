/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/issue21295.d(9): Deprecation: imports.issue21295ast_node.Visitor is not visible from module issue21295
---
*/
import imports.issue21295ast_node;
Visitor should_fail;
