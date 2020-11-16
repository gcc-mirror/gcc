// Test the scan-symbol-section directive.
// scan-symbol-section should fail with 'FAIL' if a symbol has a different
// section than expected.

// { dg-do preprocess }
// { dg-final { scan-symbol-section "dg-scan-symbol-section-1-exp-F.i" {^_test_symbol_.*$} {nomatch} } }
// The above assertion should fail with the following messages:
// FAIL: gcc.test-framework/dg-scan-symbol-section-1-exp-F.S scan-symbol-section symbol ^_test_symbol_.*$ (found _test_symbol_1) has section nomatch (found .text)
// FAIL: gcc.test-framework/dg-scan-symbol-section-1-exp-F.S scan-symbol-section symbol ^_test_symbol_.*$ (found _test_symbol_2) has section nomatch (found .data)
    .text
_test_symbol_1:
    .data
_test_symbol_2:
