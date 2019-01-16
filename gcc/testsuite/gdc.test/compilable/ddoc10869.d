// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 10869

module ddoc10869;

///
class C
{
    const
    {
        ///
        void c1Foo() const { }

        ///
        void i1Foo() immutable { }
    }

    immutable
    {
        ///
        void c2Foo() const { }

        ///
        void i2Foo() immutable { }
    }
}
