// Test the scan-symbol-section directive.
// scan-symbol-section should fail with 'UNSUPPORTED' if a symbol has no
// associated section.

// { dg-do preprocess }
// { dg-final { scan-symbol-section "dg-scan-symbol-section-2-exp-F.i" {^_test_symbol$} {^\.text$} } }
// The above assertion should fail with the following message:
// FAIL: gcc.test-framework/dg-scan-symbol-section-2-exp-F.S scan-symbol-section symbol ^_test_symbol$ (found _test_symbol) has section ^\\.text$ (no section detected)
_test_symbol:
