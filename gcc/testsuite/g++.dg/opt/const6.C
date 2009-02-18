// PR target/39179
// Make sure that we don't optimize away the load from K::k.
// { dg-options "-O2" }
// { dg-final { scan-assembler _ZN1K1kE } }

struct K {
    static const unsigned k;
};
extern "C" void abort (void);
int main() {
    if ( K::k != 1 )
      abort ();
    return 1;
}
