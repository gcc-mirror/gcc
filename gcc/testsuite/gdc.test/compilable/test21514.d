// https://issues.dlang.org/show_bug.cgi?id=21514
// DISABLED: win32 win64
/* TEST_OUTPUT:
---
compilable/test21514.d(16): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/test21514.d(16): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/test21514.d(17): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
compilable/test21514.d(17): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
compilable/test21514.d(19): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/test21514.d(19): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/test21514.d(20): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
compilable/test21514.d(20): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
---
*/

extern(C++) cdouble cpp_cadd1(cdouble c) { return c + 1; }
extern(C++) creal cpp_cadd1l(creal c) { return c + 1; }

cdouble cadd1(cdouble c) { return cpp_cadd1(c); }
creal cadd1(creal c) { return cpp_cadd1l(c); }
