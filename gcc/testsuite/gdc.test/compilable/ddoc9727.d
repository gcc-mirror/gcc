// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
module ddoc9727;

/** The function foo. */
void foo(int x);

/**  */
unittest
{
    foo(1);
}

/** foo can be used like this: */
unittest
{
    foo(2);
}

/** foo can also be used like this: */
unittest
{
    foo(3);
}
