// https://issues.dlang.org/show_bug.cgi?id=3737
/*
TEST_OUTPUT:
---
fail_compilation/fail344.d(20): Error: undefined identifier `Q`
fail_compilation/fail344.d(20): Error: undefined identifier `Q`
fail_compilation/fail344.d(20): Error: undefined identifier `V`
fail_compilation/fail344.d(23):        while evaluating: `static assert(Alike!(SIB!(crayon)))`
fail_compilation/fail344.d(23): Error: template instance `fail344.SIB!(crayon).SIB.Alike!(SIB!(crayon))` error instantiating
fail_compilation/fail344.d(23):        while evaluating: `static assert(Alike!(SIB!(crayon)))`
fail_compilation/fail344.d(28): Error: template instance `fail344.SIB!(crayon).SIB.opDispatch!"E"` error instantiating
---
*/

int crayon;

struct SIB(alias junk)
{
    template Alike(V) {
        enum bool Alike = Q == V.garbage;
    }
    void opDispatch(string s)() {
        static assert(Alike!(SIB!(crayon)));
    }
}

void main() {
      SIB!(SIB!(crayon).E)(3.0);
}
