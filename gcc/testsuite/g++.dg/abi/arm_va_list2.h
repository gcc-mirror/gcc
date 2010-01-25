// The mangling of va_list changed in GCC 4.4.  We want to warn about
// that -- but not in a system header.
#pragma GCC system_header
void f(va_list) {}
