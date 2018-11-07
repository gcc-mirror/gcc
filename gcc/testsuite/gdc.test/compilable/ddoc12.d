// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 12

int ruhred; /// This documents correctly.
int rühred; /// This should too

/**
 * BUG: The parameters are not listed under Params in the generated output
 *
 * Params:
 *     ü = first
 *     ş = second
 *     ğ = third
 *
 */
int foo(int ü, int ş, int ğ)
{
    return ğ;
}
