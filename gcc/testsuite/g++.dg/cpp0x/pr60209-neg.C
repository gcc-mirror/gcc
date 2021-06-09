// PR c++/60209
// { dg-do compile { target c++11 } }

// http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_defects.html#1473

void operator "" "boo" _ya(unsigned long long); // { dg-error "expected empty string after" }

void operator "" "boo"_ya(unsigned long long); // { dg-error "expected empty string after" }

void operator "" u"" _u(unsigned long long); // { dg-error "invalid encoding prefix in literal operator" }

void operator u"" "" _v(unsigned long long); // { dg-error "invalid encoding prefix in literal operator" }

void operator U"" "" _w(unsigned long long); // { dg-error "invalid encoding prefix in literal operator" }

void operator L"" "" _x(unsigned long long); // { dg-error "invalid encoding prefix in literal operator" }

void operator u8"" "" _y(unsigned long long); // { dg-error "invalid encoding prefix in literal operator" }

void operator u"" L"" _z(unsigned long long); // { dg-error "concatenation of string literals with conflicting encoding prefixes" }

void operator ""_p ""_q(unsigned long long); // { dg-error "inconsistent user-defined literal suffixes" }

void operator "" "" while(unsigned long long); // { dg-error "unexpected keyword; remove space between quotes and suffix identifier" }

void operator "" ""(unsigned long long); // { dg-error "expected suffix identifier" }

// { dg-error "invalid encoding prefix in literal operator" "invalid" { target *-*-* } 20 }
