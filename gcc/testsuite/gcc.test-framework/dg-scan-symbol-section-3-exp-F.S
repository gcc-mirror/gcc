// Test the scan-symbol-section directive.
// scan-symbol-section should fail with 'UNSUPPORTED' if a symbol has no
// associated section.

// { dg-do preprocess }
// { dg-final { scan-symbol-section "dg-scan-symbol-section-3-exp-F.i" {^_test_symbol$} {^\.text$} } }
// The above assertion should fail with the following message:
// FAIL: gcc.test-framework/dg-scan-symbol-section-3-exp-F.S scan-symbol-section symbol ^_test_symbol$ (no symbol detected) has section ^\\.text$
    .text
_test_wrong_symbol:
