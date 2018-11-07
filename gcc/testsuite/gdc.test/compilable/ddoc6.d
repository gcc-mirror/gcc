// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 6

/**
 *
 */
struct MyStruct(T)
{
    static if( true )
    {
        void MyStruct() {}
    }
}

void main()
{
}

/+
23
C:\code\d\bugs>dmd -D -o- 148_1.d
148_1.d(6): Error: static if conditional cannot be at global scope
+/

