// Build don't run:

// Additional sources: instantiate5.cc instantiate5-main.cc

// `global constructors' are given the same name, based on foo(), on
// both translation units, which is wrong, because it must be possible
// to define template functions in multiple translation units, as long
// as they're given the same definition

// simplified from test case submitted by Daniel X. Pape <dpape@canis.uiuc.edu>

template <class T> void foo() { }
inline int bar() { foo<void>(); return 1; }
static int i = bar();
