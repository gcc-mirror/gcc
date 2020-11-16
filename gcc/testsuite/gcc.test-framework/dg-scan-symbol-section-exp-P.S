// Test the scan-symbol-section directive.

// { dg-do preprocess }

// The .section directive changes the section for all following symbols.
    .section .text
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_function_1$} {^\.text$} } }
_test_function_1:
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_function_2$} {^\.text$} } }
_test_function_2:

// For ELF targets, the .section directive can take multiple arguments.
    .section .other_text_section,"ax",progbits
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_elf_function$} {^\.other_text_section$} } }
_test_elf_function:

// For Mach-O targets, the .section directive takes a segment name and a section name.
    .section __TEXT,__my_text_section
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_macho_function$} {^__TEXT,__my_text_section$} } }
_test_macho_function:
// Extra whitespace between .section arguments should be ignored.
    .section  __DATA , __testsection
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_macho_var$} {^__DATA,__testsection$} } }
_test_macho_var:

// The .data directive sets the section for all following symbols to '.data'.
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_var_1$} {^\.data$} } }
    .data
_test_var_1:

// The .text directive sets the section for all following symbols to '.text'.
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_function_3$} {^\.text$} } }
    .text
_test_function_3:

// The .const directive sets the section for all following symbols to '.const'.
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_data_1$} {^\.const$} } }
    .const
_test_data_1:

// Other directives do not affect the section of following symbols.
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_var_2$} {^\.data$} } }
    .data
    .p2align 2
    .size _test_var_2, 4
_test_var_2:

// Symbol name patterns can match multiple symbols, and section name patterns
// can match multiple sections.
// { dg-final { scan-symbol-section "dg-scan-symbol-section-exp-P.i" {^_test_function_} {^(\.|__TEXT,).*text} } }
