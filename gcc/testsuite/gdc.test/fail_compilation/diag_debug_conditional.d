/**
TEST_OUTPUT:
---
fail_compilation/diag_debug_conditional.d(1): Error: identifier expected inside `debug(...)`, not `alias`
fail_compilation/diag_debug_conditional.d(2): Error: identifier expected inside `version(...)`, not `alias`
fail_compilation/diag_debug_conditional.d(3): Error: declaration expected following attribute, not end of file
---
 */
#line 1
debug(alias)
version(alias)
