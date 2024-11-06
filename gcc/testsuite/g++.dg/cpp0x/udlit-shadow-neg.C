// { dg-do compile { target c++11 } }

//  Test that the standard suffixes shadow any user-defined suffixes of the same name.
long double
operator ""L(long double x)  // { dg-warning "floating-point suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator ""ULL(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

long double
operator ""l(long double x)  // { dg-warning "floating-point suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator ""ull(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""z(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""uz(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""zu(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""Z(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""UZ(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""ZU(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

//  Namespaces are no hiding place.
namespace Long
{

long double
operator ""L(long double x)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator ""ULL(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

long double
operator ""l(long double x)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return x; }

unsigned long long int
operator ""ull(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""z(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""uz(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""zu(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""Z(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""UZ(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

unsigned long long int
operator ""ZU(unsigned long long int k)  // { dg-warning "integer suffix|shadowed by implementation" }
{ return k; }

}

// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 5 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 9 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 13 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 17 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 21 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 25 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 29 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 33 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 37 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 41 }

// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 49 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 53 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 57 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 61 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 65 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 69 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 73 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 77 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 81 }
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 85 }
