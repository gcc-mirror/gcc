// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 10366

///
struct S(T)
{
    ///
    void method() {}

    public
    {
        ///
        struct Nested
        {
            ///
            void nestedMethod() {}
        }
    }
}
