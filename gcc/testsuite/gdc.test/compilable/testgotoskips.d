/*
    Tests to defend against false positives from the goto skips over decl errors
*/
// https://issues.dlang.org/show_bug.cgi?id=23271
class A {
        private static A[] active;
        private void test() {
                foreach(a; active) {
                    if(a is this)
                         goto label;
                }
                // used to say Error: `goto` skips declaration of variable `s.A.test.__appendtmp4` at s.d(...)
                active ~= this;
            label:
                return;
        }
}
