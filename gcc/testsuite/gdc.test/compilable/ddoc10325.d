// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc10325;

/** */
template templ(T...)
    if (someConstraint!T)
{
}

/** */
void foo(T)(T t)
    if (someConstraint!T)
{
}
