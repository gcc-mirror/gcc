// { dg-do compile }

typedef void (*fptr)();
fptr zeroptr = 0;
template<typename T, fptr F> struct foo { };
template<typename T> struct foo<T,zeroptr> { };
// { dg-error "not a valid template argument" "not valid" { target *-*-* } 6 } 
// { dg-error "must be the address" "must be the address " { target *-*-* } 6 }

// The rest is needed to trigger the ICE in 4.0 to 4.3:
void f() { }
foo<int,&f> m_foo;
