// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/fail_compilation -o- -w -c
/* TEST_OUTPUT:
---
fail_compilation/ddoc_18083.d(14): Warning: Ddoc: function declaration has no parameter 'this'
fail_compilation/ddoc_18083.d(14): Warning: Ddoc: parameter count mismatch, expected 0, got 1
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/
/**
Params:
  this = non-existent parameter
*/
int foo()
{
    return 1;
}
