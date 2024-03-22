// Verify that class literals are mangled the same way regardless
// of the underlying type.
// { dg-do compile { target c++2a } }
// { dg-additional-options -fabi-compat-version=0 }

struct I { int a[5], b[5], c[5]; };
template <I> struct X { };

typedef X<I{ {1,2}, {}, {11,12,13,14} }> Ti;
void f (Ti) { }
// { dg-final { scan-assembler "_Z1f1XIXtl1ItlA5_iLi1ELi2EEtlS1_EtlS1_Li11ELi12ELi13ELi14EEEEE" } }

struct C { char a[5], b[5], c[5]; };
template <C> struct Y { };

typedef Y<C{ {1,2}, {}, {11,12,13,14} }> Tca;
void g (Tca) { }
// { dg-final { scan-assembler "_Z1g1YIXtl1CtlA5_cLc1ELc2EEtlS1_EtlS1_Lc11ELc12ELc13ELc14EEEEE" } }

typedef Y<C{ "\1\2", "", {11,12,13,14} }> Tcs;
void h (Tcs) { }
// { dg-final { scan-assembler "_Z1h1YIXtl1CtlA5_cLc1ELc2EEtlS1_EtlS1_Lc11ELc12ELc13ELc14EEEEE" } }

struct S { signed char a[5], b[5], c[5]; };
template <S> struct Z { };

typedef Z<S{ {1,2}, {}, {11,12,13,14} }> Tsc;

void i (Tsc) { }
// { dg-final { scan-assembler "_Z1i1ZIXtl1StlA5_aLa1ELa2EEtlS1_EtlS1_La11ELa12ELa13ELa14EEEEE" } }
