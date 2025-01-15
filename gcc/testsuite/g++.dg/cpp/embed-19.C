// PR c++/118278
// { dg-do compile { target c++20 } }
// { dg-options "-fabi-compat-version=0" }

struct C { unsigned char a[5], b[5], c[128]; };
template <C> struct Y { };

typedef Y<C{ {1,2}, {}, {
#embed __FILE__ __limit__ (128)
} }> Tca;
void g (Tca) { }
// { dg-final { scan-assembler "_Z1g1YIXtl1CtlA5_hLh1ELh2EEtlS1_EtlA128_h(?:Lh\[0-9]*E){128}EEEE" } }

typedef Y<C{ "\1\2", "", {
#embed __FILE__ __limit__ (128)
} }> Tcs;
void h (Tcs) { }
// { dg-final { scan-assembler "_Z1h1YIXtl1CtlA5_hLh1ELh2EEtlS1_EtlA128_h(?:Lh\[0-9]*E){128}EEEE" } }
