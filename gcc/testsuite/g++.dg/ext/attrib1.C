// Test for interpretation of attribute immediately before function name.
// Origin: Joseph Myers <jsm28@cam.ac.uk>
// { dg-do compile }

// An attribute immediately before the function name should in this
// case properly apply to the return type, but compatibility with
// existing code using this form requires it to apply to the function
// type instead in the case of attributes applying to function types,
// and to the declaration in the case of attributes applying to declarations.
int ****__attribute__((format(printf, 1, 2))) foo(const char *, ...);
