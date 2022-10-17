// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -w -o- -c -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc9475;

/// foo
void foo() { }

///
unittest
{
    // comment 1
    foreach (i; 0 .. 10)
    {
        // comment 2
        documentedFunction();
    }
}

/// bar
void bar() { }

///
unittest
{
    // bar comment
}
