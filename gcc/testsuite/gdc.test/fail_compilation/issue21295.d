/*
TEST_OUTPUT:
---
fail_compilation/issue21295.d(8): Error: undefined identifier `Visitor`
---
*/
import imports.issue21295ast_node;
Visitor should_fail;
