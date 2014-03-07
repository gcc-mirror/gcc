// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

static int strlen (const char*) { return 0; }

template <int (*)(const char*)>
void f () {}

// Check that the strlen declaration here is given internal linkage by
// using it as a non-type template argument, and expecting an error.
template void f<strlen>(); // { dg-error "" "" { target { ! c++11 } } } no matching template
