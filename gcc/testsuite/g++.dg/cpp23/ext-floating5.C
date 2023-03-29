// P1467R9 - Extended floating-point types and standard names.
// IBM extended long double and _Float128 should have unordered conversion
// ranks as IBM extended long double has variable precision from 53 bits
// for denormals to more than 2150 bits for certain numbers.
// { dg-do compile { target { c++23 && { powerpc*-*-linux* } } } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-options "-mvsx -mfloat128 -mlong-double-128 -mabi=ibmlongdouble" }

auto a = 1.0F128 + 1.0L;	// { dg-error "invalid operands to binary \\\+ \\\(have '_Float128' and 'long double'\\\)" }
auto b = 1.0L + 1.0F128;	// { dg-error "invalid operands to binary \\\+ \\\(have 'long double' and '_Float128'\\\)" }
bool c;
auto d = c ? 1.0F128 : 1.0L;	// { dg-error "operands to '\\\?:' of types '_Float128' and 'long double' have unordered conversion rank" }
auto e = c ? 1.0L : 1.0F128;	// { dg-error "operands to '\\\?:' of types 'long double' and '_Float128' have unordered conversion rank" }
