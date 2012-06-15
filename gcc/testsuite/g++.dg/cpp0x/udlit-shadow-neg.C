// { dg-options -std=c++0x }

//  Test that the standard suffixes shadow any user-defined suffixes of the same name.
long double
operator"" L(long double x)  // { dg-warning "floating point suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator"" ULL(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

long double
operator"" l(long double x)  // { dg-warning "floating point suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator"" ull(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

//  Namespaces are no hiding place.
namespace Long
{

long double
operator"" L(long double x)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator"" ULL(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

long double
operator"" l(long double x)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator"" ull(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

}

// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 5 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 9 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 13 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 17 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 25 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 29 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 33 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 37 }
